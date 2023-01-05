---
title: "Password Hashing"
date: 2023-01-04T20:43:50-05:00
type: post
slug: "16-password-hashing"
tags:
  - haskell
  - fastapi
showTableOfContents: true
---

# Password Hashing
After much pain and suffering I have finally achieved password hashing in Haskell.

## Password hashing overview
To briefly summarize, you should never store raw passwords anywhere for any reason. Instead, when the user registers, the password is processed through a one-way, irreversible [hash function](https://en.wikipedia.org/wiki/Hash_function) first before it is inserted into the database. When the user attempts a login, the new password is passed through the same function and compared to the hash in the database. If they match, the original passwords must also have been the same.

A naive bad actor who snuck into your database would need to find a way to reverse the hash function to extract the original password. For good hash functions, this is practically impossible.

A cleverer, badder actor might try to calculate the hash values of common passwords (or look them up in a [rainbow table](https://en.wikipedia.org/wiki/Rainbow_table)) and check if any match the hashes in your database, but a clever developer (such as the esteemed Django devs) use a "[salt](https://en.wikipedia.org/wiki/Salt_(cryptography))": a randomized value that is appended to the password before hashing. This invalidates rainbow tables, as you would need a new table for every possible salt value.

A committed, baddest actor that hated you specifically could still try hashing common passwords with the salt listed in the DB, so the most paranoid of all developers will actually choose slower hash functions (or apply repeatedly) so that it takes longer to try out a single guess. Still, even with that inconvenient delay, short/common passwords can be guessed, so don't use bad passwords.

*(I should mention here that you should really be using [OAuth 2.0](https://oauth.net/2/) to handle authentication. It conveniently allows you to delegate identity management to a 3rd party (GitHub, Google, Facebook, Twitter, Reddit, whatever) so that users only need to have one central account, and you don't need to worry about storing passwords!)*

*(I'm not using OAuth because I don't anticipate any ~~victims~~ users other than myself, and I'd rather make a mess with password hashing than try to configure OAuth in every possible language.)*

## Password hashing in practice
In my career I haven't had to worry about this very much, because [Django does all this automatically](https://docs.djangoproject.com/en/4.1/topics/auth/passwords/). Django uses [PBKDF2](https://en.wikipedia.org/wiki/PBKDF2) & [SHA256](https://en.wikipedia.org/wiki/SHA-2) by default. SHA256 is a hash function, and PBKDF2 is an algorithm that applies a hash function to the password+salt combo repeatedly.

The FastAPI implementation did require a little more effort, but it's all just Python under the hood, so it wasn't too hard. I extracted the exact logic Django was using, pared it down aggressively, and created [`password.py`](https://github.com/go-recordkeeper/go-recordkeeper/blob/3e5ea5a87d614db52165936f333f7d3b3ad8673c/server/fastapi/goban_server_fastapi/auth/password.py).

In the process, I discovered that the hash function generates a string that looks like this:
```
pbkdf2_sha256$390000$4cjXfoHJmE2LgeLn$sSNy/30khRioV4tOIF9ZvNhInMpNHAYufEYe/CIKZ8Q=
```
This string has four parts, separated by `$`:
* `pbkdf2_sha256`: The name of the hashing algorithm
* `390000`: How many iterations of the hashing algorithm to apply
* `4cjXfoHJmE2LgeLn`: The salt
* `sSNy/30khRioV4tOIF9ZvNhInMpNHAYufEYe/CIKZ8Q=`: The hashed password

All well and good. Storing all this info lets you update your hash algorithm/settings without invalidating any old passwords.

## Password hashing in Haskell
It sucked, I did not have a good time.

I used [`password`](https://hackage.haskell.org/package/password-3.0.2.0) because it was the only package available on stackage.

`password` has a separate data wrapping type for every component in the hashing process (`Password`, `Salt`, `PasswordHash`, `PasswordCheck`, etc.). This is probably done for safety, since a type signature like `hashPasswordWithSalt :: Text -> Text -> Text` is not legible and mistakes might be disastrous, but I was annoyed with all the wrapping and unwrapping it required.

`password` also generates hashes in a similar but deceptively different format:
```
sha256:390000:NGNqWGZvSEptRTJMZ2VMbg==:sSNy/30khRioV4tOIF9ZvNhInMpNHAYufEYe/CIKZ8Q="
```
* The separator is `:` instead of `$`
* `pbkdf2` is now implicit, although `sha256` is still included
* The password hash is now [base 64 encoded](https://en.wikipedia.org/wiki/Base64) (note the trailing `==`)

This format change required its own wrapping/unwrapping functions, which involved further packing/unpacking to do the string processing because `password` uses the `Text` type instead of `String`.

### Hatin' on Haskell
Haskell is clearly built around some interesting and powerful ideas, but every time I turn around I'm hit in the face with some new gotcha.

Haskell is a clever language that allows you to write clever code. After much suffering I have learned that clever code is a necessary evil at best. Take this code that generates a random 16 byte salt:

```haskell
generateSalt :: IO (Salt PBKDF2)
generateSalt = do
  g <- newStdGen
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      ns = randomRs (0, length chars - 1) g
      strs = map (chars !!) ns
  return $ Salt $ BS.pack $ take 16 strs
```

For the uninitiated:
* `chars` is a list of characters to choose from.
* `randomRs` is standard library (technically an [external package](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html) instead of a builtin, but that's another gripe) function that generates an infinite list of random numbers in a given range.
* `ns` is an infinite list of random indexes in `chars`.
* `strs` is an infinite list of random characters from `chars`:
  * `map` takes a function and a list, and returns a list of the result of applying the function to every item in the original list.
  * `chars !!` is a [partially applied](https://en.wikipedia.org/wiki/Partial_application) function. `!!` is an infix function that gets an item from a list, so `chars !!` is a function that takes an index and returns the character in `chars` at that index.
* `take 16 strs` takes the first 16 items from `strs` and discards the rest.
* `return $ Salt $ BS.pack $ ...` does all the requisite wrapping I have already complained about.

This is all gloriously elegant, monad wrapped, functional, terse, and clever, but damn is it illegible to me. Perhaps after a few years of writing Haskell it will become instinctive and aesthetic. For now it just rubs me the wrong way.

### Praisin' Python
Consider the python equivalent:
```python
RANDOM_STRING_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
salt = "".join(secrets.choice(RANDOM_STRING_CHARS) for i in range(char_count))
```

Again, for the uninitiated:
* `join` is a method on strings that glues together a list using the string as a separator, i.e. `"-".join([1,2,3])` == `"1-2-3"`
* `secrets.choice` is a standard library function that returns a random item from an iterable (in this case, a string).
* `range(char_count)` is an iterator that counts from 0 to `char_count`.
* `... for i in ...` is a [list comprehension](https://en.wikipedia.org/wiki/List_comprehension) that creates a list containing the evaluation of the left side for every `i` in the right side.

To be fair, `"".join` looks weird until you look up the definition, and list comprehensions are slightly uncommon (although Haskell has them too, of course), but it's a one liner. If you feel marginally more verbose you can practically write it in English:

```python
RANDOM_STRING_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
salt = ""
for i in range(0, char_count):
    salt = salt + secrets.choice(RANDOM_STRING_CHARS)
```

Obviously, apples to oranges, the right tool for the job, Haskell type safety, etc etc. I'm just salty. Rant over.

## Testing it
Funnily enough, the integration tests in their previous incarnation would have been 100% A-OK without password hashing. All test data was created using the external API, so it doesn't matter to the test how the data is stored in the DB, only how the API responds to correct/incorrect passwords. Obviously, this is not acceptable.

To test properly, it is not sufficient that a single implementation respond correctly. All implementations must respond identically from the same DB state. This is the value of a having direct DB access from the integration testing environment.

*(One might be tempted to test that registrations always result in the same row in the DB, but the salt is randomized for very good reasons so that is actually impossible* ¯\\_(ツ)\_/¯ *)*

### DB access in tests
I'm not sure why I never thought of it before, but the FastAPI implementation is actually perfect for including as a test dependency. I suppose I was just hung up on Django being the reference implementation.

FastAPI is stripped down, modularized, and capable of almost everything the Django implementation is. Technically it does not have the flexibility of the Django ORM models which limits the ability to do custom queries without adding test-specific code, but that is a corner case I don't care about right now and there are solutions. In addition to DB access, FastAPI also has conveniently modularized application logic like the `password.py` module. These can be used as an expedient when verifying that other implementations behave identically.

### Writing the new test
I needed to test that all implementations generated compatible password hashes. In this case, it is sufficient to test that login succeeds when the DB contains a valid FastAPI hash. There is already a test that verifies that login in works after registration, so if login also works with the FastAPI hash, then registration must produce a FastAPI-shaped hash.

To that end, I created a new test fixture `internal_user_factory`, a counterpart to `user_factory`. While `user_factory` uses the REST API to register a new user, `internal_user_factory` uses the FastAPI implementation to create the user directly in the DB. The new test `test_login_fastapi_hash` is exactly the same as the old `test_login`, but using `internal_user_factory` instead.

## Next steps
Now that I'm thinking about it, I could just parametrize the `user_factory` fixture so that it runs tests twice, once with the REST API user and once with the DB injected user. I could add similar parametrizations for the other fixtures as well, which would provide the same compatibility guarantees basically for free. It would double the number of tests though, which would double the runtime. It's probably worth it.

Regarding Haskell, the next step is to figure out JWT signing and verification for the get current user endpoint. Once that's done I can do some refactoring, make a middleware function for auth, and get started on the actual go logic.
