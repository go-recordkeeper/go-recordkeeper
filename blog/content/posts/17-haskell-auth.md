---
title: "Authentication in Haskell"
date: 2023-01-10T19:24:07-05:00
type: post
slug: "17-haskell-auth"
tags:
  - haskell
  - pytest
showTableOfContents: true
---

# Authentication in Haskell
Accomplished.

## JWT compliance
Thanks to Haskell's incredible type safety, I learned that the way I was using [JWT](https://jwt.io/)s for authentication was not standards compliant, because it simply wasn't possible to do in Haskell. My JWT payloads basically looked like this:
```json
{
  "id": {user ID},
  "exp": {time of issue + 1 day},
}
```
As it happens, the `id` field isn't in [the RFC](https://www.rfc-editor.org/rfc/rfc7519#section-4.1). I had to change it to `sub` instead.

Additionally, Haskell's [`JOSE`](https://hackage.haskell.org/package/jose) library requires an `aud` (audience) field so that it can be checked when validating a JWT. While I was at it I tacked on the `iss` (issuer, same as `aud`) and `iat` (issued at) fields as well. Adding everything to the Django and FastAPI implementations was trivial.

## "Lenses" 🤮
Along the way, I also encountered the concept of "lenses". Haskell has "records", which have named fields, which are painful to work with. Rather than suffer, clever people invented the [lens](https://hackage.haskell.org/package/lens) library to make getting and setting fields more functional.

This is great and all and I'm happy for them, but again, it's something I don't appreciate very much about Haskell. The core of the language is infinitely(*) powerful, but users have to resort to a mishmash of third party packages to make it usable. I'd never encountered `lens` before, so while trying to get another dependency working I had to adapt to this new paradigm for working with records, something Haskell already does.

It wouldn't be so bad if this stuff was documented effectively, but the target audience for the documentation apparently isn't me. This is the introductory image on hackage and on the wiki:

[![UML Diagram](https://raw.githubusercontent.com/wiki/ekmett/lens/images/Hierarchy.png)](https://creately.com/diagram/h5nyo9ne1/QZ9UBOtw4AJWtmAKYK3wT8Mm1HM%3D)

To their credit, `lens` does seem like a nice thing to have. The use case seems to be complex nested record types, which I am not working with here. The learning curve for all the operators seems very steep, but that is pretty much par for the course with the rest of the Haskell ecosystem. I'm sure that if I get in deep enough `lens` will seem essential and obvious, but right now I'm trying to get things done and it is an unwanted and intrusive distraction.

## Using `GOBAN_SECRET_KEY` as the JWT key 
I expected environment variables to be more of a pain in the neck, but they were shockingly easy. `lookupEnv` (maybe find the variable) and `getEnv` (throw an IO exception if not available) are both in the standard library and both work in a normal IO monad. The resulting code to generate the JWT key is pretty concise:
```haskell
generateJWK :: IO JWK
generateJWK = do
  dev <- lookupEnv "GOBAN_DEVELOPMENT"
  secretKey <- case dev of
    Just _ -> pure "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj"
    Nothing -> getEnv "GOBAN_SECRET_KEY"
  let jwk = fromOctets $ T.encodeUtf8 $ T.pack secretKey
  pure jwk
```

## Wrapping up
Writing the actual code to generate and verify JWTs wasn't that interesting. I got it working after the aforementioned fuss, and even had time to refactor it and clean up the auth checking function so that it's easily usable in any Scotty request handler.

I didn't have the heart to dive into unit testing, so that's for later I guess. Everyone raves about quickcheck, but I haven't been able to identify any easily testable, non-trivial properties about any of my code so far. Perhaps ChatGPT might have some recommendations.

# Moving on: Records API
The second, considerably more significant half of the REST API deals with the actual game records. I've already nailed down the web API (scotty) and the database (hasql), so the only remaining dependency to integrate is the SGF library (tragically there seems to be only one offering on hackage and it was last updated in 2009, so hopefully it's simply good enough that no updates are needed, or I will be rolling my own). The actual move checking and capturing is all pure code, which is the perfect thing to learn pure Haskell.

## API and DB injection test fixtures
I did wind up implementing separate REST API and database injection fixtures for the integration tests so that I could test the Read endpoints before writing the Create endpoints. The move DB fixture was rather involved as I left a lot of business logic in the view function, which isn't really callable on its own, but it's not too awful.

## List Records
Trivial. The most difficult part was mapping the tuple from the hasql response to the JSON response object, but that was mostly just tedious.

## Create Record
Also trivial. Again, the most difficult part was mapping from JSON request object to hasql tuple. This was complicated by having optional fields in the request which have defaults, but nothing awful.

## Get Record
This is where I was stumped. Most of the endpoint was, again, trivial. Get is just List, but only one thing, after all. The problem is that there are two new fields, `moves` and `stones`.
* `moves` is a list of all moves played so far, which requires another hasql query, but isn't too bad.
* `stones` is a list of all stones currently on the board, which requires evaluating every move played so far. Not so easy.

For now, I just return `moves: [], stones: []` and let the tests fail while I work on the game logic.

## The rules of Go 
Basically, I need a function which simulates a series of moves on a go board of arbitrary size and returns the final board state (moves - captures) and any stones captured by the last move (not relevant for now, but will be for other endpoints). In the Python implementations I used a `Board` class which was mutated iteratively to achieve the final board state. That's obviously not gonna fly here.

Many functions involved in this implementation work will need to know about the board size. I totally could just pass that in as an argument every time, but I thought it would be cooler to encode that state in a Monad instead; that's, like, the whole point of Haskell, right? So far I haven't had the brain cells to get that working properly, but hope springs eternal.

# Total non-sequitur: Unikraft for deployment
As avid readers will now, I am using `docker compose` for dev, test, and production deployment. Dev and test are accepted standard practice, but production not so much. I excuse it because this is a toy application running on a toy computer for purposes of recreation, but a more robust deployment is an aspiration.

Just today I stumbled on [Unikraft](https://unikraft.org/), a build tool that builds your application into a minimal `.iso`s that can be run by a VM. This eliminates the OS from the deployment completely, which shaves off a significant amount of overhead. I would need to set up `qemu` or some equivalent to run the VMs, figure out a reliable orchestration scheme for getting everything communicating, and figure out my own SSL certs, but the result would be considerably more realistic.

It's not going to happen until I'm done with Haskell though, so lets just put a pin in that.

# Next steps
1. Figure out the game logic monad stuff
1. Write the rest of the record endpoints
1. Figure out proper testing in Haskell
