---
title: "Go Logic in Haskell"
date: 2023-01-17T22:15:43-05:00
type: post
slug: "18-go-logic-in-haskell"
tags:
  - haskell
showTableOfContents: true
---

I implemented the basic logic for the game of Go in Haskell. This is just a recording app, so it doesn't need to have any knowledge of kos or scoring. The only difficult bit is capturing groups that have no liberties.

## Design

I started by naively writing simple functions, layering them on top of each other. This was relatively easy, as I've more or less got the hang of Haskell syntax at this point.

There are five basic data types I wound up with:
* **`Color :: White | Black`** - Pretty self-explanatory, black stones or white stones.
* **`Pos :: Int` and `Coord :: (Int, Int)`** - Two interchangeable representations of points on the board. `Pos` is just the index if you were to count out every point on the board, left to right, top to bottom. `Coord` is an (x, y) coordinate. `Coord` is easier to reason about, as you just need to increment/decrement a dimension to find an adjacency, but `Pos` is easier to serialize and store because it is a simpler data type. I use them interchangeably and convert between them as necessary, but perhaps it would be better to refactor to only use `Pos` at some point.
* **`Board :: IntMap Color`** - A mapping of `Pos` to `Color`. `Board` stores all the stones played using the position of the stone as the key and the color as the value. If a position isn't in the mapping, then it's not on the board. The documentation for the normal `Map` recommended using `IntMap` if possible because it's drastically more efficient, I assume because no hashing is required. I don't really need the performance, but it seemed like a good reason to use `Pos` instead of `Coord`.
* **`Set Pos`** - Not a named type, but this is how I tracked groups and the set of liberties for a group.

I also wound up with a loose stack of functions that build on one another:
* **`toPos` and `toCoord`** - Convert `Pos` to `Coord` and vice versa. A little clumsy to use since you also need to pass the board size.
* **`adjacents`** - Find all the adjacent `Coord`s for a given `Coord`, respecting the edges of the board. I used a pretty slick list comprehension, pretty proud of that.
* **`buildGroup'`** - Recursively identify a set of points in a group, and a set of liberties for that group. This one is rather messy, I'd be surprised if there isn't a better way to do it. It takes a list of `Coord`s to check, pops one `Coord` off the list, and checks what's on the board at that coordinate. If it's empty, that `Coord` is added to the liberties. If it's the same color as the rest of the group, that `Coord` is added to the group and all the surrounding points are added to the list of `Coord`s to check. Finally, it recurses until the entire group is walked and there are no more `Coord`s to check. Quite a mouthful, but I couldn't think of a way to break the problem down further. This step was also the most complicated in the Python implementations.
* **`buildGroup`** - Just a nicer wrapper around `buildGroup'` to simplify the type signature. It just calls `buildGroup'` with an empty group and liberty set, and a list with a single element for the starting `Coord`.
* **`deadGroup`** - A helper for identifying dead stones. Given a coordinate, it uses `buildGroup` to check if the group at that coordinate has any liberties. If it doesn't, return the set of all `Coord`s in the group. Otherwise, return an empty set. This is useful for identifying dead stones to remove from the board.
* **`placeStone`** - Places a stone on the board. This returns the new board state, and any stones that were captured as a result.
* **`playStones`** - Calls `placeStone` recursively for a list of moves. Returns the final board state, and any captures made by the last move.

This is the purest distillate I could achieve.

## Fun with Monads

Once I got some logic written, I thought it would be appropriate to spice things up with some monads. That's the point of Haskell, right?

The first two arguments to every function were the size of the board and the `Board`. Even if they weren't relevant to that specific function, all the functions are neatly stacked on top of each other, and at the bottom, `adjacents` needs to know the size of the board so it can determine where the edge of the board is. This is a classic use case for a `Reader` monad. The immutable context is stored in the monad rather than passed as arguments, and a bind is used to extract it when necessary, like this:

```haskell
-- This is my monad
type BoardA = Reader Int

-- This is a helper that extracts the board size
boardSize :: BoardA Int
boardSize = do ask -- ask is how you get the context from a Reader monad

toCoord :: Pos -> BoardA Coord
toCoord pos = do
  size <- boardSize
  return (mod pos size, div pos size)
```

This went poorly. I initially tried to write my own monad from scratch, failed, and asked ChatGPT which described the `Reader` monad from the standard library. This worked much better, but I tried to stuff both the board size and the board itself. `Reader` is immutable, so there is no way to add stones to the board. I couldn't find a clean way to replace an old monad with a new, modified one, so I ultimately only tracked the board size in the monad.

I feel like adding the monad just for passing the board size made the design worse. It would have been better just to build a composite `Board` type that knows about the size of the board and the stones on the board, then pass that.

## Error handling
This is the first use case I've actually felt like monads were the right tool for the problem! I had already made all the functions monadic, so all I had to do was make my `GoError` type describing the failure states and compose the `ExceptT` monad transformer with the old `BoardA`:
```haskell
-- old
type BoardA = Reader Int
-- new
type BoardA = ExceptT GoError (Reader Int)
```
I also had to add error handling logic at the highest level because `playStones` could now fail, but that is expected and required when adding errors. Crucially, I didn't have to modify any of the rest of my logic in any way, which was awesome!

## Testing
So far I have yet to be impressed with testing in Haskell. I'm using [hspec](https://hackage.haskell.org/package/hspec), which provides the traditional TDD testing assertion interface. [HUnit](https://hackage.haskell.org/package/HUnit) seems like another well established framework, but fundamentally it's just a different way of organizing your tests.

I was looking forward to using [QuickCheck](https://hackage.haskell.org/package/QuickCheck), but I have yet to identify a place where it would be really useful. The concept of property based testing has been helpful, and Haskell definitely makes it easy to accomplish, but QuickCheck itself doesn't seem useful yet. It seems designed to fuzz over prohibitively big input domains (like strings or arbitrary lists), but Go boards are small enough it is easier to just iterate over every board position. Ah well.

I've noticed that idiomatic tests in other languages tend to be very linear, with a setup, assertion, and teardown. Haskell is designed around function composition, so it is very easy and encouraged to tailor tests with all kinds of helpers and iterators. I'm not sure if that's a good thing for legibility, but it definitely helps expressivity.

I also haven't found a good way to unit test the internals of a module without exporting them. Functions like `buildGroup` aren't really useful outside the `Go.hs` module, but if I were to write tests for them, they would still need to be exported. Right now I'm just exporting everything, but I'm not happy about it.

One big win is that code coverage is shockingly easy, easier than any other testing environment I've ever tried. You literally just run `stack test --coverage`, and it generates all the coverage files and an HTML summary.

## Takeaways
* The Haskell implementation wound up being about %50 bigger than the Python implementations, but most of that was the qualified imports and exports, not code. The Python implementations had practically no dependencies and were pretty much just pure Python, but Haskell is highly modularized, and you need to pay for that when importing. Haskell is also strictly typed, so some extra verbosity is expected. Honestly, I expected it to be longer. In Java, it probably would be.
* I'm still disappointed with how undiscoverable Haskell functions are. There are a massive number of useful functions but very specific like `lift`, `liftM`, `liftA2`, `mapM`, `mapM_`, `foldM`, `foldM_`, `foldlM`, `foldrM`, etc. that are difficult to live without, but very hard to find. I have had great success with asking ChatGPT for suggestions whenever I get stuck and inferring the concepts I am missing from its answers, but that hardly seems like a consistent way to learn a language.
* There's this whole philosophy of functional programming that pure functions are easier to reason because they have no side effects. This is true and is great and pure functions are objectively better, but I still find Haskell functions difficult to reason about from a syntactic perspective. Understanding a function requires understanding *all* the functions it is composed of, which is difficult considering how generic most Haskell functions are. Consider the type signature for `foldM`: `(b -> a -> m b) -> b -> t a -> m b`. I have nothing against `foldM`; it's important and it works and I couldn't do much without it. My meager brain just has difficulties reasoning about it while trying to fit it into the problem I am trying to solve. Maybe I'm just too crusty.

## Next steps
Wire up some endpoints!
