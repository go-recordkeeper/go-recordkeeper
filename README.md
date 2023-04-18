# go-recordkeeper
![Vue](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/vue.yml/badge.svg)
![Django](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/django.yml/badge.svg)
![FastAPI](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/fastapi.yml/badge.svg)
![Haskell](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/haskell.yml/badge.svg)
![Rust](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/rust.yml/badge.svg)

An application to record go games. Try it out at [go.chiquit.ooo](https://go.chiquit.ooo/)!


## Why record go games?
Because I want to review them after I play them.

I wanted a way to record games played in person on my phone, but the apps I tried were hard to use on a touchscreen. This application is built specifically to cater to my needs, which are to record, store, review, and export games. AI review is tragically outside the scope.

## There has to be a better way
There almost definitely is. My ulterior motive was to have a relatively simple web application that I could reimplement in various languages and platforms. I'm the only anticipated user, so I can set my own requirements and timelines, and I get to sharpen my skills through repetition. "Recording go games" is a pretty minimal set of features for a website, but it's still a meaningful step up from "Hello, World".

So far, I've finished three implementations:

* [Python + Django](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/django) - Written using [Django](https://www.djangoproject.com/) and [Django REST Framework](https://www.django-rest-framework.org/). The original, reference implementation. I am also using the built-in migration system to manage DB schema changes, since I don't want to keep migrations synchronized across all the implementations.
* [Python + FastAPI](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/fastapi) - Also written in Python, but using [FastAPI](https://fastapi.tiangolo.com/) for the REST stuff and [SQLAlchemy](https://www.sqlalchemy.org/) for the DB. So far this is my favorite :)
* [Haskell](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/haskell) - Written in [Haskell](https://www.haskell.org/) using [scotty](https://hackage.haskell.org/package/scotty) for the REST API and [hasql](https://hackage.haskell.org/package/hasql) for the DB. It's a powerful language, but the build times are atrocious.

OpenAPI specs are provided for [FastAPI](https://go.chiquit.ooo/fastapi/redoc) and [Django](https://go.chiquit.ooo/django/swagger/). Because I took some shortcuts on the Django implementation, the FastAPI API is more canonical.

## Architecture
I'm more interested in API services than traditional web template stuff, so I decided to make it a single page application. The website consists of three parts:

* The [web app](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/client/vue), written in Vue.
* The [API servers](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server), in all their glory.
* The Postgres database.

The web app sends requests to the server using the REST API, which updates the database accordingly. The REST API and the database schema are both well-defined, so as long as the various server implementations conform to those expectations, they are interchangeable.

An extra benefit of having interchangeable servers is that they can all use the same [integration tests](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/test). For extra uniformity, every server implementation has a `Dockerfile` for easy containerization. The integration test code invokes docker directly to bring the various server containers up and down as needed, and to manage a postgres container to provide the DB. This enables effective black box testing while still providing control over the test data.

## Deployment
For some extra credit, I decided to manage the deployment myself on my own hardware rather than using the free tier of a PaaS like Heroku. I had some Raspberry Pis lying around that are ample for my user base. The "proper" tool to manage a containerized deployment would be something like Kubernetes, but my understanding is that the primary value proposition of the heavy-duty containerization tools is scalability, which is not part of my use case. Instead, I'm just using Docker Compose to manage the various API servers.

An interesting caveat of using Raspberry Pis is their truly abysmal processing power. It is actually more than sufficient for running the services since there is practically no load on the API server, but it is quite problematic for the build process. Pis have ARM processors, so Docker images built on x86 architectures are not compatible. So far I have just been building the images on the Pi, but it is quite slow. I did decide to just commit all the static resources directly into the [repository](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/deploy) so that build step can be run in my development environment.

I'm also using nginx as a frontend proxy for all my locally hosted servers, SSL termination, and static file hosting.

## Blog
I have also been keeping a [blog](https://go.chiquit.ooo/blog/) chronicling my efforts. There is no intended audience for it, it's more of a meditative retrospective for me to maintain focus on what I've been working on.
