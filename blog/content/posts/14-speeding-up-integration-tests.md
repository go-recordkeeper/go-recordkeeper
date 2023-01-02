---
title: "Speeding Up Integration Tests"
date: 2023-01-01T15:07:48-05:00
type: post
slug: "14-speeding-up-integration-tests"
tags:
  - pytest
---

It's been a hot minute! I've been working on a few different projects for the last month:
* [Advent of Code](https://adventofcode.com/2022) (my [solutions](https://github.com/dchiquito/advent-of-code-2022))
* [configurator](https://github.com/dchiquito/configurator), a CLI tool for corralling system configuration files into a repository. It makes it much less painful to coordinate configs between multiple machines, especially:
* Setting up [Neovim](https://neovim.io/). My [`init.lua`](https://github.com/dchiquito/configurations/blob/main/home/.config/nvim/init.lua) is still a work in progress, but nvim now does almost all the things that VSCode does. The level of configurability feels much more granular, and configuration is done imperatively rather than declaratively, which feels more powerful. It feels cool using the keyboard to do everything, but I don't know that it's necessarily faster, considering how many VSCode shortcuts I already use. On the other hand, it's definitely much less intuitive, and all the vim-specific and personalized keybindings mean that any habits I build aren't going to transfer to anywhere else. Setting it up was also a multi-week endeavor, and things still aren't seamless. Python formatting isn't happening, the python LSP doesn't recognize the poetry virtual environment, treesitter causes an error whenever a buffer is closed, and there are still staple plugins that I haven't gotten around to trying out yet. I just tried to add a word to the spellcheck dictionary and it threw an error. It's nice that all the guts are exposed and there is definitely a solution to every possible problem, but it's really painful that things don't just work out of the box.

Anyway, with the help of [ChatGPT](https://en.wikipedia.org/wiki/ChatGPT) I was able to stabilize the Haskell implementation a bit. I needed a bit of general direction to get off the ground which I would normally ask a subject-matter expert for, but in lieu of that, ChatGPT was very helpful. It provided pretty accurate code snippets that demonstrated standard library features I didn't know about, it helped identify sources of errors that I couldn't fathom, and it even recognized code that broke with different versions of the same library. It wasn't perfectly knowledgeable of all things, but it was enough to get me unstuck.

With some very rudimentary Haskell code written, I wanted to start testing it in the integration testing harness. Tragically, the current suite took a full minute to run, which is honestly rather atrocious. As you may recall from previous blog posts, test data was created by **(1)** using `subprocess` to invoke **(2)** a `manage.py` script using the poetry shell, which ran **(3)** some fixture-specific Django ORM code which creates the data. Test cleanup was accomplished similarly.

A minute is too long for 16 tests. After some soul-searching I found that all the fixtures were doing was possible using existing API endpoints, so I just replaced all of that subprocess code with API calls. Every DB row insert went from ~1 second to 0.15 seconds, which really adds up when each test generally needs at least a user and a record.

I also replaced the `manage.py` script that was being used to reset the database with some raw SQL which accomplishes the same thing. It requires that `psql` be installed on the client machine, but I'm realistically the only person who will ever run these tests, and it's really really fast, so that's all right.

I previously expounded on the benefits of direct DB access, and I think there are still valid points there. I think the fastest possible solution would be invoking the Django ORM models directly in the test environment, but that requires a Django app context which is its own kettle of fish. If I ever need finer control over the DB than the API can provide, I will need to finally figure that out.

The only really impactful way to speed up tests more is to parallelize them. `pytest` doesn't support that out of the box and it will likely require some additional care when writing tests since full database resets are no longer an option, so that is a problem for another day.
