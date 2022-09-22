---
title: "Introduction to Go Recordkeeper"
date: 2022-09-21T18:55:29-04:00
type: post
slug: introduction
tags:
  - roadmap
---

I like to play the board game [go](https://en.wikipedia.org/wiki/Go_(game)). Personally, I find that go is more conversational than adversarial. You and your opponent will both have ideas and opinions, you'll ask each other questions, find surprising answers, be refuted or vindicated. Of course, at the end of the game someone wins, but that's just the topic; for me, the joy is in the discussion.

After finishing a game, it's sometimes nice to go over the highlights with your opponent, or use an AI to get the "objectively" right answers. When playing on a go server reviewing the game is trivial, but when playing in person on a physical board, you need to record the game yourself with pencil and paper, an app, or on a website. After struggling to use [OGS](https://online-go.com/) on my phone (otherwise a fantastic site, I highly recommend it), I decided that I would build a game recorder that suited my needs.

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

Instead of building out more features I don't really need, my plan is to rewrite the existing app using new technologies and languages I'd like to learn. Go Recordkeeper is very simple as web applications go, and I've already built it once, so rebuilding it should be relatively easy (lol).

This blog exists to document that journey and to practice the fine art of technical writing. My motives are mostly selfish, but if any of it ever proves useful to anyone else, that'd be pretty cool.
