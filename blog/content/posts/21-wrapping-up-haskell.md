---
title: "Wrapping Up Haskell"
date: 2023-02-07T21:50:18-05:00
type: post
slug: "21-wrapping-up-haskell"
tags:
  - haskell
  - docker
showTableOfContents: true
---

I've been procrastinating on the "final" Haskell post until I reached a point with some closure, so there's quite a list.

## Download
The only remaining API endpoint is the download endpoint. This is non-trivial because it requires converting the record details and move list into a `.sgf` file.

I first tried using the [`sgf`](https://hackage.haskell.org/package/sgf) hackage package. Sadly it wasn't included in the stackage (Haskell has two package repositories; anyone can upload anything to [hackage](https://hackage.haskell.org/), while [stackage](https://www.stackage.org/) is a curated subset of hackage packages that are known to work). After installing directly from hackage, I learned that it just didn't compile with my (stable) GHC.

Fortunately, writing my own SGF file turned out to be easier than expected. SGF files are very general and support basically arbitrary metadata and move information. My particular use case is much more limited. From the previous Python implementation, I already know exactly what I want the file contents to be, so it was easy enough to just stitch it together myself with string concatenation. There is definitely a more efficient way to accomplish it, but it works and only takes ~50 LOC.

With a function that spits out valid SGF files, it was trivial to make a new endpoint that just returned that content with the appropriate headers. All the endpoints are done!

## Configuration
At this point I realized that I was still hardcoding the DB connection information for testing purposes, so the code was undeployable. This was also a surprisingly easy fix:
```haskell
import qualified Data.ByteString.UTF8 as BSU
import qualified Hasql.Connection as HC

startApp :: IO ()
startApp = do
  dbName <- BSU.fromString <$> getEnv "POSTGRES_NAME"
  dbUser <- BSU.fromString <$> getEnv "POSTGRES_USER"
  dbPassword <- BSU.fromString <$> getEnv "POSTGRES_PASSWORD"
  dbHost <- BSU.fromString <$> getEnv "POSTGRES_HOST"
  let connectionSettings = HC.settings dbHost 5432 dbUser dbPassword dbName
```

hasql requires UTF8 encoded byte strings (yet another Haskell string type 🤮), so the only mildly interesting part was remembering the `<$>` fmap operator to apply the type conversion to a monad.

## Refactoring
With all the code written, the logical next step is to reevaluate it and tidy it up a bit. I obviously squashed all the warnings and tidied up some edge cases and removed all the leftover comments, but I was too lazy to properly docstring every function. None of that is particularly interesting, though.

The good stuff was the reorganization. The only pure code I have is the game logic and the SGF formatter; both of those are already highly isolated, reasonably polished, and good enough for me. Pretty much all the other code is endpoints, which is all structured pretty similarly:

* Data types for JSON request/response
* The SQL queries, in hasql-th form
* The handler, generally shaped like this:
  ```haskell
    create :: HP.Pool -> ScottyM ()
    create pool = post "/api/records/" $ do
      ... logic ...
  ```

The JSON types and SQL queries are already in their simplest form and ungeneralizable. There are similarities between endpoints, but I don't think any JSON types or SQL queries are actually duplicated wholly. I could have hypothetically centralized the SQL queries and created a handful of general use queries that do everything, but that would be less efficient than the carefully optimized queries I have now, and would violate the whole vertical slice architecture.

The only place where substantial refactoring is even possible is in the endpoint logic itself. Scotty has proven to be very friendly and concise; All the scotty code I have written is basically already as simply as possible.

All the actual logic itself is obviously highly endpoint specific, so that just leaves invoking the SQL queries. As this example from the `create` endpoint shows, this is rather verbose:

```haskell
-- insert is the INSERT INTO ... query function
-- row is a tuple of data to insert into the DB
result <- liftIO $ HP.use pool $ HS.statement row insert
case result of
  Right recordId -> do
    status status201
    json $ toResponse recordId row
  Left err -> do
    raiseStatus status500 $ "DB Error."
```

It gets worse when we need to make multiple queries at once, like fetching a record and its moves:

```haskell
recordSelect <- liftIO $ HP.use pool $ HS.statement (userId, recordId) selectRecord
movesSelect <- liftIO $ HP.use pool $ HS.statement recordId selectMoves
case (recordSelect, movesSelect) of
  (Right record, Right moves') -> do
    ... useful code ...
  (Left (HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0)))), _) -> do
    raiseStatus status404 "Does not exist."
  _ -> raiseStatus status500 "DB error."
```

This code runs both queries and proceeds with the rest of the endpoint if both succeed. The `selectRecord` function is a `singletonStatement`, which means it throws an error if nothing is returned. If this exact error happens, a 404 message is appropriate. Otherwise, a generic 500 error.

These patterns occurred in every endpoint, so it made sense to refactor them into a helper function, `execute`.

```haskell
isDebug :: IO Bool
isDebug = isJust <$> lookupEnv "GOBAN_DEVELOPMENT"

execute' :: (HP.UsageError -> ActionM b) -> HP.Pool -> S.Statement a b -> a -> ActionM b
execute' errorHandler pool query args = do
  result <- liftIO $ HP.use pool $ HS.statement args query
  case result of
    Right value -> return value
    Left err -> errorHandler err

defaultErrorHandler :: HP.UsageError -> ActionM b
defaultErrorHandler err = do
  debug <- liftIO isDebug
  case err of
    HP.SessionError (HS.QueryError _ _ (HS.ResultError (HS.UnexpectedAmountOfRows 0))) -> do
      raiseStatus status404 "Does not exist."
    _ -> do
      if debug
        then raiseStatus status500 $ TL.pack $ show err
        else raiseStatus status500 "DB error."

execute :: (Show a, Show b) => HP.Pool -> S.Statement a b -> a -> ActionM b
execute pool query args = do
  liftIO $ print args
  result <- execute' defaultErrorHandler pool query args
  liftIO $ print result
  return result
```

`execute` takes the connection pool, a query to run, and the argument tuple, and returns result in a scotty `ActionM` monad. If the query fails, `defaultErrorHandler` is called, which will return a 404 or 500 error as appropriate. It will even check if the deployment is in `DEVELOPMENT` mode and provide a more helpful error message if so.

The `execute'` function is also available for endpoints that need to handle errors differently. For example, the login endpoint needs to return a 401 instead of a 404 if there is no row in the DB for the given username.

With the help of `execute`, the two-query example above is simplified to:
```haskell
record <- execute pool selectRecord (userId, recordId)
moves' <- execute pool selectMoves recordId
... useful code ...
```

That's a lot less boilerplate, and a lot less indentation. I'm quite proud of that.

## Bugs
On to deployment!

Shockingly, despite its type system, Haskell code can contain bugs.

### JSON validation
As soon as I tried to actually test the Haskell backend with the frontend client, I ran into a JSON serialization issue. Logging in and creating default games worked fine, but as soon as you started editing fields before creating a record, the request would fail.

Apparently all input fields will always return strings, even in TypeScript with typed refs. The initial komi and handicap values (`7.5`, `0`) are both `number`s in JS, but as soon as you modify the text field they are converted to strings (`"7.5", "0"). It seems django-rest-framework and pydantic are permissive enough to cast these strings back to the appropriate numerical type while validating, but Haskell's [aeson](https://hackage.haskell.org/package/aeson) is not.

This is actually not a Haskell bug, but a client bug. I resolved it by casting the text field values in the client before sending the request.

### Suicide
As soon as I started clicking around on the game board, I noticed some weird behavior with captures. Sometimes, but not always, suicidal moves were allowed, which put any subsequent moves into a weird state where logic ceased to apply.

Put simply, the `placeStone` function follows these steps:

* Verify that the position is on the board
* Verify that there is not already a stone at that position
* Put the stone on the board
* For each adjacent position:
  * Check if there is a stone there
  * If so, identify the group it belongs to and count the groups liberties
  * If liberties == 0, add all the stones in the group to a list of stones to capture 
* Remove any captured stones from the board 
* If there were no captures, identify the group the new stone belongs to, count its liberties, and throw an error if liberties == 0

Can you spot the error? It is obnoxiously subtle.

When checking if adjacent positions are captured, I neglected to stipulate that they must be of the opposite color. This is fine if a single stone is played suicidally, which is incidentally the only case I tested. If, however, a move was played that killed a whole group, the capture test would dutifully capture all the stones, including the one that was just placed. Finally, there was indeed a capture, so the final liberty check is not done, which would have caught the error when there was no group to check the liberties of.

It was a simple fix, but an obnoxious bug to hunt. Perhaps there is a more lucid way to write the code, but I suspect not. Some logic is just complicated.

### CASCADE deletes
In the Django ORM, there is a handy flag on all foreign key fields called `on_delete` which specifies the behavior when deleting the row a foreign key refers to. The recommended default (which I used) is `on_delete=CASCADE`, which will cause a "cascade" of deletes in order to preserve referential integrity.

I use this cascading deletion behavior to make deleting records easier. When a record is deleted, it causes all the moves that point to that record to be deleted implicitly, which is great. Without that `CASCADE` constraint, the database would complain about referential integrity and stop the record delete from happening in the first place.

Unfortunately, I was bamboozled.

The Django ORM `on_delete=CASCADE` is implemented completely in the Django ORM, despite the identically named postgres constraint. There are [reasons](https://code.djangoproject.com/ticket/21961) for this, but I was certainly mislead.

The upshot is that I had to rewrite the deletion endpoint:
* Check if the record exists, if not, 404
* Delete all the moves for the record
* Delete the record

This is substantially more awkward than just attempting the deletion and 404-ing if no such record could be deleted. An unfortunate unforeseen consequence of using Django.

Fun fact, this is actually also a bug with the FastAPI implementation. I just never tested it, somehow. Deletes work fine on records with no moves, which is why the integration tests were passing.

## Deployment
All the code is written and the bugs are squashed, so it's time to deploy to production!

Deployment failed utterly.

;_;

Adding the `haskell` profile to the docker compose and nginx server block was easy. Running `docker compose build` on the Raspberry Pi was not.

Firstly, my network connection was somewhat unstable. Building a docker image involves pulling a lot of data from the internet. I had problems with `apt`, `pip`, and `stack` all aborting prematurely whenever a packet dropped. My solution was to run `docker compose build haskell` to avoid distracting the CPU cores as much as possible, then just rerunning it until it got past the `stack update` step.

The next step in the image was to build the dependencies only.

This took 6 hours.

And failed.

This step does admittedly take ~2 minutes on my laptop, which is pretty long. I have no justification as to why it would take four orders of magnitude longer on the Raspberry Pi.

The actual build failure seemed to be related to the ARM build of some networking package, so not exactly something that be worked around.

My options are to run the build on my laptop targeting ARM and uploading the image, or to throw out docker compose entirely and try a different deployment mechanism ([Unikraft?](https://unikraft.org/)), or even a different server entirely.

I lack the motivation to do any of those things, so for now I have admitted defeat and disabled the Haskell implementation entirely. Tragic.

## Next steps
Build up some courage to get Haskell working, I guess.

I was being very deliberate with the Haskell implementation because I wanted to learn Haskell. My next prospective languages are Rust and Java, which I am already quite familiar with. I'm thinking of just feeding my existing implementations into ChatGPT and asking it to do the lifting, since I care less building up my own knowledge of those languages. It would certainly speed things up a bit, and our AI overlords do seem to be imminent. Learning to effectively use a language model is probably a better use of my time than rehashing a language I already know.
