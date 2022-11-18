---
title: "Introducing Haskell"
date: 2022-11-17T22:05:47-05:00
type: post
slug: "11-introducing-haskell"
tags:
  - haskell
showTableOfContents: true
---

It's Haskell time, oh boy!

I've been amped to do something practical with Haskell since I found [Learn You a Haskell for Great Good](http://www.learnyouahaskell.com/). It's available in full online, but it's so good I bought it just to have it.

## The good

Haskell is a pretty cool language.

* **It's 100% functional.** Mutating state isn't just discouraged, it's not allowed. Every function is a a pure function with no side affects.
* **It's got a powerful algebraic type system.** It's strongly typed, so all those beautiful functions are rigorously defined. Data is constructed out of other data. [Monads](http://learnyouahaskell.com/a-fistful-of-monads) help you conveniently wrap/unwrap data inside/outside of other data as needed.
* **It's lazy.** Everything's a function and there's no global state to synchronize, so nothing needs to actually be called until it's needed. Everything is lazy evaluated, and the compiler can optimize accordingly.

Haskell has a reputation for requiring that the developer learn a new and radically different paradigm from the C-style procedural family of languages.

## The bad/ugly

**hot take alert**
Haskell is (so far, at least) not very nice to use.

* **Packaging sucks.** [Cabal](https://www.haskell.org/cabal/) is the standard way to do Haskell packaging. But it sucks, so someone made a better thing called [Stack](https://docs.haskellstack.org/en/stable/) which uses it's own CLI and configuration files, but it actually just generates cabal configuration files and delegates to cabal???
* **Package installation sucks.** The standard package repository is the [Hackage](https://hackage.haskell.org/). But apparently conflicting dependencies were such a headache to deal with that someone made [Stackage](https://www.stackage.org/), which has a subset of Hackage packages that are known not to break eachother???
* **Installing it sucks.** The `ghc` (Glasgow Haskell Compiler), `cabal`, and `stack` can all be installed with your package manager, but I learned the hard way that you actually need to use `ghcup` to install everything or you're going to have a bad time. I very carefully followed the [ghcup instructions](https://www.haskell.org/ghcup/install/) to install everything and the [stack instructions](https://docs.haskellstack.org/en/stable/GUIDE/) to set up my project, but I fear the day I need to update something with a breaking change.
* **The specification is fuzzy.** Haskell was conceived by committee in 1990, and evolved as features were tacked on to the de facto implementation, the GHC. Rather than a single, central, canonical language, Haskell has "Language Features" that can be switched on/off globally or per file (`{-# LANGUAGE TemplateHaskell #-}` 🤮). A good compromise leaves everyone unhappy, especially me.

## My experience

I got the [implementation](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/haskell) all set up with the [stack instructions](https://docs.haskellstack.org/en/stable/GUIDE/).

### Servant
I chose [Servant](https://www.servant.dev/) as my web framework, purely because the example snippet they advertized was the simplest and had the least boilerplate from all the alternatives listed on [the wiki](https://wiki.haskell.org/Web/Frameworks).

Servant uses types to define the properties of the endpoint. The API definition for the register endpoint looks like this:
```haskell
type RegisterAPI = "register" :> ReqBody '[JSON] RegisterRequest :> PostCreated '[JSON] RegisterResponse
```
* `:>` is a "combinator" that glues properties of the API together.
* `"register"` means the URL for the endpoint is "/register/". `"foo" :> "bar"` would mean "/foo/bar/".
* `ReqBody '[JSON] RegisterRequest` means the request body is JSON serialized and is shaped like the data type `RegisterRequest`.
* `PostCreated '[JSON] RegisterResponse` means the endpoint is a `POST`, returns `201 CREATED` when successful, and the response body contains a JSON serialized `RegisterResponse`.

All your endpoint definitions can then be glued together to create a single type that represents your entire API. Pretty nifty.

### Register, Login, and Get User

To start with, I decided to stub out the the auth endpoints: register, login, and get the current user. It's where I started with the FastAPI implementation, and I thought that went well.

Defining the basic shape of the endpoints with Servant was surprisingly easy. Of course everything needs to be sliced vertically, which required using Haskell's module system, which I had a harder time with. It feels clumsy because I'm unfamiliar with it, but I can't articulate any real pain points so it must be pretty good. I will say that VSCode's "Go to definition" function doesn't work on any non-local modules, which is a real bummer. Apparently this is an [HLS bug](https://github.com/haskell/haskell-language-server/issues/708), so it's not just VSCode.

### Middleware misery

I had a much harder time getting the authentication middleware set up. Servant does support JWT authentication out of the box, but I had a rough time applying it to the get current user endpoint. After much wailing and gnashing of teeth, I found the correct incantation, so I'm going to chalk up my sufffering to insufficiently beginner friendly documentation and (obviously) my own inexperience. It's not compliant with the other implementations yet, but at least everything's plugged in.

After getting the basics set up I managed to refactor it down substantially and cut out a fair bit of boilerplate; I think the current implementation is pretty clean. So far Haskell's function-ness has been obnoxious because of its novelty, but I can definitely smell how powerful it is. The temptation to write clever code is intoxicating; I'm quite looking forward to polishing the rest of the endpoints.

## Next steps

* **Tests.** Apparently [HUnit](https://hackage.haskell.org/package/HUnit-1.6.2.0#readme) is the de facto testing library, so getting that set up and running is the next thing to do.
* **Docker.** The application needs to be deployed in Docker eventually, and it needs to be running in docker compose to connect to the Postgres DB. This will be my first time setting up non-python containers from scratch, so it should be interesting. Python containers are nice because if you mount the source as a volume, you only need to rebuild the image when the dependencies change. Presumably this is not the case in Haskell :(
* **Configuration.** Because all functions are pure, I/O is, shall we say, special. Managing configuration will be quite different from languages I'm familiar with, so that will be interesting.
* **Postgres.** I'll need to pick out and install a Postgres driver and figure out how to do all that I/O crap inside the endpoint handlers. I can't imagine that Haskell ORMs are very ergonomic, so I anticipate working with raw SQL. Should be interesting.