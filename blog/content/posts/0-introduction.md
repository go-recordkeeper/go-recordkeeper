---
title: "Introduction to Go Recordkeeper"
date: 2022-09-21T18:55:29-04:00
type: post
slug: introduction
tags:
  - roadmap
---

I like to play the board game [Go](https://en.wikipedia.org/wiki/Go_(game)). Personally, I find that Go is more conversational than adversarial. You and your opponent will both have ideas and opinions, you'll ask each other questions, find surprising answers, be refuted or vindicated. Of course, at the end of the game someone wins, but that's just the topic; for me, the joy is in the discussion.

After finishing a game, it's sometimes nice to Go over the highlights with your opponent, or use an AI to get the "objectively" right answers. When playing on a Go server reviewing the game is trivial, but when playing in person on a physical board, you need to record the game yourself with pencil and paper, an app, or on a website. After struggling to use [OGS](https://online-go.com/) on my phone (otherwise a fantastic site, I highly recommend it), I decided that I would build a game recorder that suited my needs.

[So I did](https://go.chiquit.ooo/).

Go Recordkeeper is designed to:

- Record go games
- Work well on mobile
- Export .sgf files
- Review games afterward
- Deploy easily
- Flex my web dev skillz

That's it. It doesn't do much, but it does it fairly well.

For next steps, it would be fun to plug in [Katago](https://katagotraining.org/) and get AI reviews, or to build a full-fledged variation branch explorer, or to let users play eachother online. However, there are already [plenty](https://online-go.com/) [of](https://ai-sensei.com/) [other](https://pandanet-igs.com/communities/gopanda2) [existing](http://www.tygemgo.com/) [services](https://www.gokgs.com/) that do that better. So, what to do with my copious free time?

### The plan

Instead of building out more features I don't really need, my plan is to rewrite the existing app using new languages I'd like to learn. I'm specifically interested in server applications (all frontend frameworks are just Javascript), so as long as the new servers are compliant with the reference implementation, then they should just plug right in to the web client and the database. Go Recordkeeper is very simple as web applications go so it shouldn't be too terribly timeconsuming to reimplement, but it still includes basically everything a "real" REST API would need. In every new language, I will need to figure out:

- **Web servers** - There are plenty of web frameworks out there, but setting them up is definitely non-trivial.
- **Databases** - The application obviously needs to read and write data to the database.
- **Authentication** - The reference implementation specifies a simple API and password hashing scheme.
- **The rules of Go** - Determining when a group of stones has been captured is a pure algorithmic problem.
- **Tests** - Compliance with the specification is impossible without testing.

Each implementation should be a drop-in replacement for every other. Ideally, I'd like to deploy them all simultaneously and load balance between them

### But why?

Yeah, I admit I'm not going to have very much to show for all my effort at the end of the day. I'm basically building solutions to a problem I have already solved.

On the other hand, this is a great way to focus specifically on learning a new language. Whenever I've started learning a new language in the past, I'm either doing it because a language is the best way to solve a problem I have, or out of pure interest in some feature of the language. When I have a problem, understanding and conceptualizing it is always a time consuming distraction from adapting to the new language. However, when I don't have a problem, I don't have any reasons to actually write code.

Since I'm the only stakeholder and I've already written Go Recordkeeper once, I have a very firm understanding of the requirements, the API surface, and the design principles involved. Since I already have the answers, I don't need to waste any time or effort on the bigger picture; I can just write code.

This blog exists to document that journey and to practice the fine art of technical writing. My motives are mostly selfish, but if any of it ever proves useful to anyone else, that'd be pretty cool.
