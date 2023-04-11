---
title: "Rusting"
date: 2023-04-11T11:33:10-04:00
type: post
slug: "24-rusting"
tags:
  - rust
  - axum
---

The Rust implementation is finished! Surprise!

Usually it takes several blog posts to finish an implementation, but Rust was quite straightforward. It definitely helped that I was already conversant in Rust, and I had a decent template from the Haskell implementation.

## Workflow
The implementation plan I've settled on looks something like this:

1. *Hello World* - Get the language installed.
1. *Choose your frameworks* - Specifically the REST API framework and the postgres library.
1. *Translate password hashing* - Practically all endpoints require the user to be logged in, so we need to be able to register and log in before we can test anything else. So far I have yet to find two `pbkdf2` libraries in any language that use the same hash format, so I need to manage the hashing/unhashing myself.
1. *Set up auth endpoints* - Stub out the register, login, and get current user endpoints, which involves getting the REST API framework working.
1. *Get/Set JWT* - Figure out the language's JWT library, and how to get the `Authorization` header idiomatically in the web framework.
1. *Database* - Set up the postgres library, copy the necessary SQL statements, and plug them into the endpoints. Also figure out how to read environment variables to set up the database connection in a configurable way.
1. *Dockerize* - Get the app building in a Docker image. This is highly language dependent. For Rust and Haskell I found it very useful to use multistage builds to minimize image size, which I assume applies to similar compiled languages..
1. *Integration tests* - Add the docker image to the `tests/docker-compose.yml` and run tests. Fix bugs until the tests pass.
1. *Create/List Record endpoints* - These endpoints don't involve moves, and so don't require any knowledge of the rules of go. Just a bunch of CRUD boilerplate. Take the opportunity to refactor and generalize helpers as appropriate here.
1. *The rules of Go* - Write the code that identifies captures and illegal moves. These are essentially pure algorithms, so this is the best chance to get down and dirty with the nuts and bolts of the language. I haven't had much success copying this code from past implementations, and writing it from scratch is funner anyway. Don't forget unit tests, you are guaranteed to have missed some corner cases.
1. *All other endpoints* - This actually goes really fast. There's nothing new being done here, just more CRUD endpoints and SQL queries.
  * *...and SGF download* - This sucks, actually. All I really need is a valid SGF file, so I've just been using string templates/concatenation to construct the contents of the file. It's pretty ugly, but it works, so ¯\_(ツ)_/¯
1. *CI* - Copy an existing CI file, figure out what actions you need for your language, and add a badge to the `README.md`. In different circumstances I'd say you should do this sooner, but since it's just me writing a feature all at once from scratch, the CI is mostly a seal of approval and a reminder if I need to revisit the implementation in the future.
1. *Deploy* - After all the integration tests pass, slap the new entry in the `deploy/docker-compose.yml`, add the URL to the nginx config, and add the new implementation to the selector in the client. Easy as that!

## Rust specifics
Not a lot to say honestly, I just followed the script.

For my frameworks, I chose [axum](https://docs.rs/axum/latest/axum/) and [tokio_postgres](https://docs.rs/tokio-postgres/latest/tokio_postgres/index.html). I opted not to use an ORM since I already had all my SQL queries typed out, and translating between JSON-serializable objects and ORM-serializable objects is rather obnoxious. If I were actively making changes to the API it would be more tempting.

While I was writing the password hashing code, I some problems with the `pbkdf2` crate taking ~10 seconds to hash a password. My settings require 39,000 rehashes, but that's still an egregiously long time. It turns out that running `cargo build --release` instead of `cargo build` is quite important.

Building the final result on the Raspberry Pi was once again a painful experience. Every build would start experiencing network failures about 20 seconds in to pulling the cargo dependencies. Fortunately, others have felt my pain. There is a cargo configuration option that pulls individual files over HTTP as needed, rather than cloning a whole git repo for every repository. Add the following to `.cargo/config.toml`:

```toml
[registries.crates-io]
protocol = "sparse"
```

I heard something to the effect that this will be the default behavior soon (possibly in Rust 1.69?), so this point may be moot.

## Takeaways
Rust is great! Axum and tokio_postgres both worked well, there were crates that did basically everything I needed (shoutout to [serde_json](https://docs.rs/serde_json/latest/serde_json/)), and everything pretty much just worked. The downside is that I didn't learn anything particularly revelatory, but I had a good time. 

## Next steps
I've got a backlog of issues to chew through, so that should be done soon. I'm also thinking of rehashing the API a bit to make it a little saner; some fields serialize moves coordinates as `{x:...,y:...}`, and some use an integer index. We'll see what strikes my fancy.
