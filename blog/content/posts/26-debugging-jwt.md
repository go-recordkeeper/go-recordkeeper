---
title: "Debugging JWT"
date: 2023-04-22T17:02:17-04:00
type: post
slug: "26-debugging-jwt"
tags:
  - haskell
  - django
  - fastapi
  - rust
  - jwt
showTableOfContents: true
---

The last remaining significant bug, and also the very first GitHub issue, is a [JWT compatibility problem](https://github.com/go-recordkeeper/go-recordkeeper/issues/1). All the implementations use the same authentication API, so hypothetically they should be able to use each other's JWT auth tokens. When I added the ability to switch the implementation in the UI, I tested that each implementation worked in isolation. However, sometimes when switching implementations, you would be spontaneously logged out. This isn't a huge problem as you can simply log in again, so I decided (correctly) that it would be a PITA to solve, and filed an issue to deal with it later.

Now it is later, so it's time to fix it. The debugging process was considerably more involved than any of the general tidying I was doing, so this gets its own post.

## The problem
The first step was to isolate exactly when and how the bug occurs. At this point there are four implementations, so there are sixteen different ways to log in to one implementation and then try to use that auth token in another (or the same) implementation. This is the table of where the bugs are, columns being the login service and rows being the usage service:

|  | Django | FastAPI | Haskell | Rust |
|--------|--------|--------|--------|--------|
| Django | ✔ | ✔ | ✔ | ✔ |
| FastAPI | ✔ | ✔ | ✔ | ✔ |
| Haskell | ❌ | ❌ | ✔ | ❌ |
| Rust | ✔ | ✔ | ❌ | ✔ | 

This indicates that the Haskell server works with itself, but will not accept any other server's JWTs. Also, Rust won't accept Haskell's JWTs either. Because there's no obvious bingo here, there are probably multiple errors at play. At least we have some degree of confidence that Haskell is involved somehow.

## Reproducing the problem
As a general rule, I've found that when confronting a new, untested bug, it's useful to have a failing test for it before doing anything else. A test lets you figure out precisely what the failure conditions of the bug are, provides an easy way to reproduce the problem while trying to isolate it, gives you confidence that your fix works, and lives on in the test suite forever to prevent regressions.

This is the first integration test that involves multiple services, so I had to rearchitect the testing harness a bit. Originally, I had an autouse pytest fixture that would invoke a `docker compose` up command before every test, parametrized so that it ran each test once per implementation. The docker compose services were all bound to port 8000, so all the tests had to do was send their requests to `http://localhost:8000/...`. 

This new JWT compatibility test involves having multiple services running simultaneously, so I had to refactor things a bit so that each service has its own unique port, and the test client sends traffic to the correct port for the implementation being tested. With that done, I realized there wasn't really a point to bringing services up/down between tests, so now all services are started at the start of every test run. This cuts down substantially on the number of moving parts and improves the test runtime a bit.

Sadly, this change had the unintended side effect of drastically slowing down integration testing in CI. Where previously only the service being tested needed to be built into a Docker image, now all services need to be built, which approximately quadruples the runtime and will only get worse as more implementations are added. It's "just" CI so it's not the end of the world, but it is painful. The only real solution that comes to mind is building all the images in parallel as a separate workflow and uploading to a Docker hub. I don't really enjoy doing all that fiddly devops configuration, so I'm going to procrastinate on that.

The actual test isn't very interesting, it just logs in with one server and tries to use the auth token on a different server.

## Finding the problem
Having a test did indeed make debugging very easy. Sprinkling some prints revealed that Haskell was rejecting the Django and FastAPI JWTs because of deserealization errors.

For the uninitiated, [JWTs](https://jwt.io/) are basically just little blobs of JSON data that are cryptographically signed by the server and handed out to clients. The client holds on to the JWT and provides it to the server whenever it needs to provide proof of identity. The server checks that the signature is correct, which proves that the server signed that blob of JSON in the past. JWTs are cool because the server doesn't need to keep a ledger of clients to have a session with them; it can offload that bit of data storage to the client in the payload of the JWT.

The [JWT specification](https://www.rfc-editor.org/rfc/rfc7519#section-4) is quite precise about what is and isn't allowed in the payload. The fields all have specific names, types, and semantics. I had previously run afoul of this when I tried to include the user ID in the JWT by setting the `id` field. It turns out that the correct field name is `sub`, and various frameworks refused to cooperate with me until I renamed it.

### `sub`
In this case, the problem was that Haskell implemented the specification with great exactitude; it refused to accept any `sub` that was not a string. Django and FastAPI both grabbed the user ID from the database as an `int` and used that as the `sub`. The Django and FastAPI implementations were apparently all sufficiently permissive, but Haskell was not. Regardless, the fix was easy.

### `iat` and `exp`
The Rust implementation was working because it allows arbitrary payloads, and I naively typed `sub` as an `i32`. In addition to that, I had also typed the `iat` ("Issued At") and `exp` ("Expiration Time") fields as `u64` instead of `f64`. These timestamps are technically required to be "NumericDates", which apparently includes floats? Regardless, the fix was to fix those type signatures and add some type casts.

### Leeway
Fixing all the type errors resolved all the bugs in the matrix above. Sadly, this only revealed a new bug: most of the time (but not all the time), Django and FastAPI would fail when trying to use JWTs from Haskell or Rust. The problem was not reproducible manually at all, it only happened when running the test, which pointed to a race condition of some kind.

More debug prints indicated that the Python implementations were rejecting the JWTs because the `iat` field was *after* the current time, a temporal impossibility. However, when I compared all the timestamps, they were in the correct order. The `iat` field was always *before* the current time at verification.

Because the Python service code was running in a container, I didn't want to spend the time and effort figuring out exactly what was happening in the guts of the JWT library I was using. My hypothesis is that the Python JWT library works with integer timestamps, while the Haskell and Rust libraries use floats. The [RFC](https://www.rfc-editor.org/rfc/rfc7519#section-2) says "JSON numeric value", which does technically include floats. Somehow, when rounding the float `iat` field, it lost some resolution and got an `iat` value that was after the current time. That shouldn't be possible since `int()` always rounds down, but it's my best guess.

Regardless of the root cause, I fixed it by adding two seconds of leeway to the verification step. Apparently clock synchronization is a ubiquitous problem, enough so that a `leeway` field is conveniently available on the `jwt.decode()` method.

## Moral of the Story
Tests are great, love my integration testing harness.
