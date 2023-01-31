---
title: "Ticking Boxes With Haskell"
date: 2023-01-31T12:54:00-05:00
type: post
slug: "20-ticking-boxes-with-haskell"
tags:
  - haskell
---

I've finished implementing all the "interesting" record related endpoints in Haskell:
* Playing moves
* Passing
* Undoing moves
* Deleting records
* Updating records

The process was pretty mechanical:

1. Copy a similar endpoint
1. Rename the functions, add the new endpoint to `Records.hs`
1. Copy the request/response from the FastAPI serializers (if necessary)
1. Write the necessary SQL statements in `hasql-th` format
1. Translate the endpoint logic from the FastAPI endpoint
1. Run integration tests
1. Iron out bugs until tests pass

The only real obstacles were:
* Realizing that the position field in the moves table is nullable, to represent passing. The core game logic needed to be tweaked a bit, but nothing invasive.
* The ordering of the `removals` field in the response to the undo endpoint was relying on implicit ordering, so that had to be made explicit, and the tests needed updating.

## Thoughts
Most of the code is dedicated to converting data types between the hasql API, the JSON API, and the game logic API. It would be nice to separate that from the business logic somehow.

The hasql-th queries are effective, concise SQL. The only problem is that they output tuples, which are tedious and verbose to consume and translate. I have yet to find a better alternative, sadly.

There was surprisingly little overlap in the required SQL statements for each endpoint. I could just make a single "fetch the entire record" endpoint and hide all the SQL, but most of the data would be wasted most of the time. I like the specificity, but not the associated verbosity.

## Next steps
The only remaining Haskell endpoint is downloading the SGF file. That will require either figuring out the API for a library, or writing my own, either of which are a substantial chunk of work.

Now that (almost) all the Haskell code is written, it would be a good time to do some refactoring and tidying. The strict compartmentalization I've been doing also makes it easier to identify untested branches, so I'll probably tack on some more integration tests.
