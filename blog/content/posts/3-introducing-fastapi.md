---
title: "Introducing FastAPI"
date: 2022-10-05T20:11:48-04:00
type: post
slug: "introducing-fastapi"
tags:
  - fastapi
  - sqlalchemy
  - poetry
showTableOfContents: true
---

I decided that my [first alternative implementation](https://github.com/go-recordkeeper/goban-server-fastapi) will be using [FastAPI](https://fastapi.tiangolo.com/). FastAPI is purportedly a very lightweight and very fast Python web framework. While Django is "batteries included", FastAPI is all about stripped down minimalism. 

I know I hyped up learning funky new languages, but FastAPI is a new framework to me and I will also get some practice good practice reimplementing Django stuff in a somewhat familiar environment where copy/pasting is easy. I predict that future implementations will have a lot more in common with goban-server-fastapi than they will with goban-server-django.

# The plan

The plan for this implementation is to use [FastAPI](https://fastapi.tiangolo.com/) for the web framework (with closely coupled [Pydantic](https://pydantic-docs.helpmanual.io/) for validation), [SQLAlchemy](https://www.sqlalchemy.org/) as the ORM, and the exact same PostgreSQL database+schema used in the Django implementation. The objective is having a second implementation of the server that can act as a drop-in replacement for the reference implementation from the perspective of the web client, even if it is not 100% identical.

FastAPI, as previously mentioned, is the hip thing to get things done quick in Python. SQLAlchemy seems to be the most popular and mature ORM outside of Django, so this is a good opportunity for me to broaden my Python horizons a bit while dipping my toes into this whole reimplementation thing. Hopefully I can do a lot of copy-pasting for the boring bits.

# Also introducing Poetry

Incidentally, I completely forgot about [Poetry](https://python-poetry.org/) when setting up [goban-server-django](https://github.com/go-recordkeeper/goban-server-django), much to my chagrin. Poetry is a Python dependency manager/packager in the style of [Yarn](https://yarnpkg.com/) or [Cargo](https://doc.rust-lang.org/cargo/). The classical alternative (which I used for goban-server-django) is to write a `setup.py` file describing your package, installing things directly with `pip`, and managing your own virtual environments (I use [virtualenvwrapper](https://virtualenvwrapper.readthedocs.io/en/latest/)). Poetry is generally a nicer experience, although it is still rather young and there are some gotchas.

# First steps

Installing all the aforementioned dependencies and setting up the basic hello world stuff was pretty straightforward. As promised, FastAPI is quite lightweight.

### docker compose

The complications ensued when trying to set up the docker compose environment to test the database connections. The desire is to have the alternative implementations interface with the database schema from the reference implementation, specifically so I don't have to create the schema and manage migrations separately in every project. This means that weirdly, the docker compose configuration needs to reference the goban-server-django docker image so that it can initialize the database.

I wound up adding a [build-image.sh](https://github.com/go-recordkeeper/goban-server-django/blob/main/build-image.sh) script to goban-server-django that would build the Django image and tag it appropriately, which makes it available as a docker image to any other docker compose configurations running on the same machine. In production, this hack is irrelevant because the docker compose configuration will have all implementations available.

### Dependencies: just say no

Handling docker images that way is kinda janky and I probably should use a container registry like [Docker Hub](https://hub.docker.com/) or [GitHub Container Registry](https://github.com/features/packages), but so far the only cloud service I am using is GitHub. [goban-js](https://github.com/go-recordkeeper/goban-js), used to render the game board on the web app, isn't even published on npm; I'm just installing it directly from the repository. I kinda like not having those external service dependencies so I'm not reliant on staying within the free tier on anything. I'm also free to move faster since I don't have to worry about versioning published versions and pushing breaking changes or whatever.

### Poetry in Docker

I also ran into some problems writing the [Dockerfile](https://github.com/go-recordkeeper/goban-server-fastapi/blob/main/Dockerfile) for the FastAPI implementation. With the traditional `setup.py` approach, installation in the Dockerfile is as simple as `RUN pip install .`. Poetry is not included by default in the Python docker image, so `pip install "poetry==1.2.1"` (or whatever the lateset version is) has to be done before anything else. Since right now I only care about running in development, I opted to use `poetry install` directly in the Dockerfile rather than `poetry build`ing a wheel and `pip install`ing that. That required disabling Poetry's default virtual environment and specifying a bunch of other flags required for Docker compliance. It's pretty messy and I need to make another pass at it before deploying to production.

# Authentication

After getting everything plugged in, it's time to write some business logic. Since you need to be logged in to do anything, I decided to start with the authentication system.

As a refresher, there are three endpoints:
1. **POST /login**: Log in an existing user. Returns an authorization token.
1. **POST /register**: Create a new user, then log them in
1. **GET /user**: Returns some information about the currently logged in user.

Additionally, `GET /user` needs to detect the `Authorization: Bearer ...` header on the request and do all the requisite authorization checks.

To no one's surprise, the FastAPI code was refreshingly minimal. The only mildly interesting part was using [Dependency](https://fastapi.tiangolo.com/tutorial/dependencies/) as a middleware to check the Authorization header. FastAPI is a very succinct wrapper around [Starlette](https://www.starlette.io/), which has [it's own middleware system](https://www.starlette.io/middleware/). I was initially mislead, but FastAPI dependencies work pretty well. I will say that they seem like they would become quite verbose in larger applications, but obviously that is not the target audience.

Also to no one's surprise, I had to write a substantial amount of code to get SQLAlchemy working. It's a powerful, fully featured ORM, so that's to be expected. It's not really interesting code so there's not much to say ¯\\\_(ツ)_/¯

### Shameless theft

Implementing `/user` endpoints was straightforward; it's just some careful inspection of the Authorization header (conveniently copied from goban-server-django) and a database lookup.

The fun part was working with passwords. For my own convenience I used the default Django settings wherever possible, which means the password was hashed using [PBKDF2](https://en.wikipedia.org/wiki/PBKDF2). Rather than adding a dependency on Django just for this one password hash (remember: just say no to dependencies), I peeled the Django hash encode/decode implementation out of the [source code](https://github.com/django/django/blob/c58a8acd413ccc992dd30afd98ed900897e1f719/django/contrib/auth/hashers.py#L289) and dumped it into [auth.py](https://github.com/go-recordkeeper/goban-server-fastapi/blob/35ff617ac3f5b1946523dba31af44612cb2408be/goban_server_fastapi/auth.py). It's ugly, but for now it works. I will need to trim it down once I have some tests so that I have a convenient reference when implementing it in other languages.

# Next steps

As of now I have authentication endpoints and SQLAlchemy models for all the relevant tables. The next steps include:
1. Adding [pytest](https://docs.pytest.org/) for unit testing.
1. Adding the rest of the endpoints (much easier with a test framework).
1. Writing an integration test suite. If all implementations need to behave identically, we obviously need to test that. It will be a very interesting problem.
1. Prepping for production (gunicorn, all settings, docker image, etc.).
1. Deploying alongside goban-server-django, with nginx load balancing.
