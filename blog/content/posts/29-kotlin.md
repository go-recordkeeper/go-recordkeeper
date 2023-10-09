---
title: "Progress + Kotlin"
date: 2023-10-09T11:59:36-04:00
type: post
slug: "29-kotlin"
tags:
  - kotlin
  - nvim
  - bash
  - typescript
showTableOfContents: false
---

It's been a few months! I've been busy playing [Factorio](https://www.factorio.com/), which scratches all the same itches as programming.

I did make some progress I forgot to document, so let's go over that first.

## Calculating score
After finishing a game, it's nice to be able to know who won. In go this is a rather involved process involving lots of arithmetic. You need to remove any dead groups, count all the empty intersections you control, add any prisoners you took, and add any handicap+komi to get your final score, which you can compare with your opponents. Doing this calculation manually is tedious, and entering the final results once you have them is not a good UX, since you have to edit the game info.

As an aside, many go boards offer score estimation to get a feel for who is generally ahead/behind. This is much more subjective and difficult, so I'm not touching that. I am only interested in score at the end of the game.

The solution I landed on involves a new button that opens up a score calculator view of the board. On this view you cannot add or remove stones, so you must have completely filled in all the borders before it can be used. Selecting a group will toggle its status between alive and dead, regardless of whether or not it actually has two eyes. These expedients shift the responsibility for a whole passel of edge cases to the user, which is great for me as the implementor. Once you have resolved the board state, you can confirm the results and save the winner to the DB. It's not really displayed anywhere, but it is exported to SGF and at least the information is there for the future.

All this logic was surprisingly easy to just roll into the client side code. The most interesting part was probably rendering territory as belonging to one side or the other. Everything else was just bookkeeping and some API calls.

## Bash implementation
Yeah, this was a terrible idea. I got far enough to confirm that it is in fact possible, but manipulating data is so grueling that there's no way I'm ever finishing it. Basically the only data types bash has are strings and arrays, so every operation on data involves awk/sed/grep, which is frankly disgusting.

It's cool that it's possible though.

## Kotlin
I'm working on Kotlin instead. It's been a hot minute since I've touched the Java ecosystem, so I thought a refresher would be good. I decided on Spring as the web framework because it's so ubiquitous, and I frankly didn't fully understand the generic web framework when I used it in the past. I'm going to try using the lowest level Postgres driver possible so I can recycle the SQL statements I've already settled on rather than rewriting everything in an ORM like Hibernate.

I used https://start.spring.io/ to bootstrap the project. I wanted to use Java 21 since it's the freshest, but sadly it isn't bundled in `apt` yet, so I settled on 17 as it is the previous LTS.

The only major pain point in the bootstrapping process was getting [kotlin-language-server](https://github.com/fwcd/kotlin-language-server) to work in neovim. As far as I can tell, it just defaults to Java 8 instead of doing any kind of intelligent assessment of the correct version from the project files. I had to explicitly set `kotlin.compiler.jvm.target` to 17 in my `init.lua` when loading the plugin, which is a frustrating exception to need to include.

## Next steps
More Kotlin, coming soon! I've done a tiny bit of Kotlin in the past, but I don't remember anything about the syntax, so I'll probably knock out a few Hello World's and get cooking on the checklist.
