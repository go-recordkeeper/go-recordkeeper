# go-recordkeeper
![Vue](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/vue.yml/badge.svg)
![Django](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/django.yml/badge.svg)
![FastAPI](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/fastapi.yml/badge.svg)
![Haskell](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/haskell.yml/badge.svg)
![Rust](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/rust.yml/badge.svg)
![Deno](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/deno.yml/badge.svg)
![Kotlin](https://github.com/go-recordkeeper/go-recordkeeper/actions/workflows/kotlin.yml/badge.svg)

<p align="center">
<img src="https://go.chiquit.ooo/logo.png" width="200" />
</p>

An application to record go games. Try it out at [go.chiquit.ooo](https://go.chiquit.ooo/)!


## Why record go games?
Because I want to review them after I play them.

I wanted a way to record games played in person on my phone, but the apps I tried were hard to use on a touchscreen. This application is built specifically to cater to my needs, which are to record, store, review, and export games. AI review is tragically outside the scope.

## There has to be a better way
There almost definitely is. My ulterior motive was to have a relatively simple web application that I could reimplement in various languages and platforms. I'm the only anticipated user, so I can set my own requirements and timelines, and I get to sharpen my skills through repetition. "Recording go games" is a pretty minimal set of features for a website, but it's still a meaningful step up from "Hello, World".

So far, I've finished four implementations:

* [Python + Django](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/django) - Written using [Django](https://www.djangoproject.com/) and [Django REST Framework](https://www.django-rest-framework.org/). The original, reference implementation. I am also using the built-in migration system to manage DB schema changes, since I don't want to keep migrations synchronized across all the implementations.
* [Python + FastAPI](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/fastapi) - Also written in Python, but using [FastAPI](https://fastapi.tiangolo.com/) for the REST stuff and [SQLAlchemy](https://www.sqlalchemy.org/) for the DB. So far this is my favorite :)
* [Haskell](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/haskell) - Written in [Haskell](https://www.haskell.org/) using [scotty](https://hackage.haskell.org/package/scotty) for the REST API and [hasql](https://hackage.haskell.org/package/hasql) for the DB. It's a powerful language, but the build times are atrocious.
* [Rust](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server/rust) - Written in [Rust](https://www.rust-lang.org/) using [axum](https://docs.rs/axum/latest/axum/) for the REST API and [tokio-postgres](https://docs.rs/tokio-postgres/latest/tokio_postgres/) for the DB. Architecturally it's practically identical to the Haskell implementation, but I'm much fonder of Rust as a language.
* [Deno](https://deno.com/runtime) - Written in [TypeScript](https://www.typescriptlang.org/) using the deno runtime as opposed to the more established [Node.js](https://nodejs.org/en). Deno ships with a surprisingly complete web server API, so I opted to use it directly instead of a more traditional web framework.
* [Kotlin](https://kotlinlang.org/) - Written in Kotlin, run on the JVM, and using [Spring Boot](https://spring.io/projects/spring-boot) as the web server. Spring is a very mature and rather opaque ecosystem unto itself, so I did most of the DB, authentication, and middleware myself rather than fighting Spring into the shape I wanted.

OpenAPI specs are provided for [FastAPI](https://go.chiquit.ooo/fastapi/redoc) and [Django](https://go.chiquit.ooo/django/swagger/). Because I took some shortcuts on the Django implementation, the FastAPI API is more canonical.

## Architecture
I'm more interested in API services than traditional web template stuff, so I decided to make it a single page application. The website consists of three parts:

* The [web app](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/client/vue), written in Vue.
* The [API servers](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/server), in all their glory.
* The Postgres database.

The web app sends requests to the server using the REST API, which updates the database accordingly. The REST API and the database schema are both well-defined, so as long as the various server implementations conform to those expectations, they are interchangeable.

## Deployment
For some extra credit, I decided to manage the deployment myself on my own hardware rather than using the free tier of a PaaS like Heroku. I had some Raspberry Pis lying around that are ample for my user base. The "proper" tool to manage a containerized deployment would be something like Kubernetes, but my understanding is that the primary value proposition of the heavy-duty containerization tools is scalability, which is not part of my use case. Instead, I'm just using Docker Compose to manage the various API servers.

An interesting caveat of using Raspberry Pis is their truly abysmal processing power. It is actually more than sufficient for running the services since there is practically no load on the API server, but it is quite problematic for the build process. Pis have ARM processors, so Docker images built on x86 architectures are not compatible. So far I have just been building the images on the Pi, but it is quite slow. I did decide to just commit all the static resources directly into the [repository](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/deploy) so that build step can be run in my development environment.

I'm also using nginx as a frontend proxy for all my locally hosted servers, SSL termination, and static file hosting.

## Testing

### Unit tests
Each implementation maintains its own set of unit tests. I initially tried to unit test all the code, but it proved to be painfully tedious to continuously rewrite the same exhaustive unit tests in each language. I ultimately settled on unit tests for the code related to the core game logic, since that is the most intricate, algorithmic, and error-prone.

### Integration tests
Because all the server implementations have the same API surface, they can all use the same [integration tests](https://github.com/go-recordkeeper/go-recordkeeper/tree/main/test), which makes life much easier. The integration test harness uses Docker Compose to bring the various server containers up and down as needed, and to manage a postgres container to provide the DB. The integration tests are the arbiter of implementation compliance, which is the only reason I feel like good unit tests are redundant.

### Browser tests
I did not and will not write any. There is a time and a place for browser testing, and this is neither. If the application was an order of magnitude larger, had more user stories than a single QA person could check in a reasonable time, had massively more development activity, and had a substantial user base who would be negatively impacted by regressions, then I would consider it.

## Blog
I have also been keeping a [blog](https://go.chiquit.ooo/blog/) chronicling my efforts. There is no intended audience for it, it's more of a meditative retrospective for me to maintain focus on what I've been working on.
