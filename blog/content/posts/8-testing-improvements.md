---
title: "Testing Improvements"
date: 2022-11-11T15:43:48-05:00
type: post
slug: "8-testing-improvements"
tags:
  - django
  - pytest
---

### bad submodules :'(

I have come to discover that git submodules are not the be-all and end-all, tragically. I'm using them in [goban-server-test](https://github.com/go-recordkeeper/goban-server-test) to reference the various goban-server implementations, and it works fine when testing stuff on `main`. It sadly does not work well for testing local unco,mitted code. It is possible to set up a local directory as a git remote, set up a branch for local development, iteratively commit changes, and then pull them inside the submodule, but it's not easy and it's certainly not fun.

To that end, I added a `.env` file to `goban-server-test` which contains two variables, `GOBAN_TEST_DIR_DJANGO` and `GOBAN_TEST_DIR_FASTAPI`. These point to the submodules by default, but can be re-targetted to wherever the developer has code to test. The `docker-compose.yml` now refers to these variables instead of hardcoding the submodules when building and mounting the code. I've found it to be a much more easy and ergonimic way of handling it.

### Better data management

While I was at it, I tried to speed up the DB reset code. Previously it was invoking `docker compose run --rm django python manage.py migrate` or variations on that theme to manipulate the database between tests. This was attrociously slow, since it had to set up a new container for every command.

To mitigate this, I installed `goban-server-django` inside the `goban-server-test` python environment, so all that Django code is available directly within the test context. Then you can just do all the magic directly in the test fixtures.

Sadly, after some wrangling I could not figure out a clean way to actually load the Django context to the point where ORM models were accessible directly. I suspect the cannonical way would be to set up `goban-server-test` as a Django project and install the `goban-server-django` stuff as an [application](https://docs.djangoproject.com/en/4.1/ref/applications/), but I am lazy and that sounds complicated. I'm sure there is a way to load Django correctly, but again, I am lazy.

In lieu of any proper solutions, I simply replaced `docker compose run --rm django python manage.py` with `poetry run python goban-server-django/manage.py`. This cuts out the overhead of docker, which is substantial. I'm still invoking [shell_plus](https://django-extensions.readthedocs.io/en/latest/shell_plus.html) to run python code as strings to generate test data, but one step at a time.

Sadly, the performance improvement was not as substantial as I'd hoped. The current 14 tests originally ran in ~1 minute 30 seconds, but now run in ~1 minute. Substantial, but not what I was hoping for. I suspect that using [`reset_db`](https://django-extensions.readthedocs.io/en/latest/reset_db.html) and `migrate` to reset the database between runs is the culprit. I'm confident there are further optimizations that can be pursued later.
