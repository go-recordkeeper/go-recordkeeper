---
title: "Skipping Haskell"
date: 2022-12-02T17:44:48-05:00
type: post
slug: "13-skipping-haskell"
tags:
  - haskell
showTableOfContents: true
---

I just discovered this fun tidbit of Haskell knowledge:
```haskell
baz = do
  let foo1 = "bar" -- fine
  let foo2 = do "bar" -- weird, but fine
  let foo3 = (do "bar") -- still fine
  let foo4 = (
    do "bar"
  ) -- parse error (possibly incorrect indentation or mismatched brackets)
```

I cannot figure out how to nest `do` blocks in a readable manner. I'm tired of having to wrestle Haskell every time I want to refactor something so I think I'm going to drop it for a bit and work on the Rust implementation instead.
