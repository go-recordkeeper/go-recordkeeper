---
title: "Integration Testing Harness"
date: 2022-10-30T20:23:47-04:00
type: post
slug: "5-integration-testing-harness"
tags:
  - django
  - pytest
  - docker
showTableOfContents: true
---

I'm back from vacation and it's time to get testing. I need a test harness that can verify any goban-server implementation is correct. Every test should be run against a real service with a real database behind it. 

## The plan
I already have one and a half goban-server implementations written in python and deployed with docker-compose. Therefore,

* Services being tested are started/stopped using docker-compose.
* Tests are written in Python+pytest.

Unfortunately, this has some corolaries:

* Test data is injected by running shell commands on the `django` container. (?)
* [subprocess](https://docs.python.org/3/library/subprocess.html) is used to invoke docker-compose to start/stop services and modify the DB. (??)

Orchestrating test environments and organizing test data are perhaps the two hardest problems in automated testing, especially so for integration tests.

## Wow, that sounds janky
Yeah, it is. Fortunately, it all works at the end of the day, so ¯\\\_(ツ)_/¯

## git submodule
The very first obstacle is how to find the code to test. All of the goban-server implementations live in their own repo, but we want all of them available in the [goban-server-test](https://github.com/go-recordkeeper/goban-server-test) repo so that we can build and deploy them for testing purposes.

I've seen [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules) before and heard about how scary they are, but honestly they are a pretty elegant solution to exactly this problem. Submodules allow you to include a child git repository as a directory within a parent git repository. On the file system, they exist as a self-contained repository; you can `cd` in to the child and do whatever git magic you want there. In the parent repository, that directory is tracked as a commit tag; running `git submodule update` will automatically do a `git fetch` from within the child and update the tag accordingly.

In this case, I set up submodules for the `goban-server-django` and `goban-server-fastapi` repos. Now you simply have to run `git submodule update` to pull the latest changes from the implementation repos and run tests against them. You can also add alternative git remotes to the submodules so you aren't forced to push code to master before you test it. All in all, very clean.

## docker compose
To start with, the [goban-server-test](https://github.com/go-recordkeeper/goban-server-test) `docker-compose.yml` contains a `postgres` service defining the DB, and a service for each goban-server implementation (currently just `django` and `fastapi`). The implementation services each have their own `profile`, which informs docker-compose that that service should not be started by default. This lets us use a single file to define all of the test environments.

## 🪄 magic 🪄
The secret ingredient is pytest fixtures. They are beautiful, magical things and I can't imagine writing tests without them.

This is our testing lifecycle:

* The testing session begins. Delete everything in the docker-compose environment: `docker compose down`
* For each service (`django` or `fastapi`) (we'll assume `fastapi` for illustrative purposes):
  * Start it in the background, implicitly starting `postgres` as well: `docker compose up -d fastapi`
  * For each test function:
    * Wipe the database: `docker compose run --rm django python manage.py reset_db -c --noinput`
      * There's a lot going on there, so lets step through it:
      * `docker compose run --rm django`: Start a fresh `django` container, run the remainder as a shell command, then delete the container
      * `python manage.py`: `manage.py` is Django's CLI interface. There are several useful subcommands defined by default and by various plugins.
      * `reset_db -c --noinput`: `reset_db` is defined in the `django-extensions` plugin and basically just drops all your tables. `-c` tells it to close all sessions first, and `--noinput` forces it to skip prompting the user for confirmation.
    * Set up all the DB tables again: `docker compose run --rm django python manage.py migrate`
    * Execute the test
  * Stop the service: `docker compose stop fastapi` (fun fact, this leaves `postgres` running)
  * Delete the service container: `docker compose rm fastapi`

The entire test environment is contained in docker-compose, so we can simply invoke all of those `docker compose` commands using `subprocess.run` and everything pretty much just works. That entire workflow is defined in four relatively concise fixtures.

There are sadly some assumptions made that the user has all the docker compose stuff installed correctly and isn't running anything on the required ports, but hey, you can't have everything.

## Test data, generallly
As previously mentioned, one of the perennial problems of integration/end-to-end tests is how to get test data into the system. Generally speaking, there are several options, even for a simple service+DB system like this one:

1. **Inject SQL directly to the DB**. This is bad because you have coupled your test code directly to your schema, and you need to write SQL by hand. Any schema changes require re-verifying that all test data is still valid.
1. **Use SQL dumps to establish a baseline state**. Before every test, reset the DB to some known state that contains all the data you could possible need. Practically speaking, that's not possible; what if you want to test no rows in a table? Also, every new test will probably require new data, which involves changing the baseline, which possibly breaks old tests.
1. **Invoke the service API**. This requires your API to be powerful enough to create all the data you might need for testing, which practically never happens. What if you want to test some strange corrupted-data edge case? It can also be slow if you need large volumes of test data.
1. **Secret testing API**. Create secret, hopefully testing-only endpoints that empower tests to create whatever data they need. This requires you to change your application to enable testing, creates a very dangerous backdoor that needs to be locked down appropriately, and introduces untested, ghostly spaghetti code into your service.
1. **Use service code in test setup**. Import the service directly into the test and hijack the internal implementation details to create the test data you need. This breaks the illusion of black box testing, or at least warps it since you are hypothetically only doing it to set up test data. Your tests are now coupled to implementation details, and your application needs to be engineered to enable that thievery.

There is no good solution, just least bad solutions.

I will say that my favorite testing setup I have ever done is 5) using service code in test setup. Django's powerful ORM made it easy to simply import models from the model layer and use them to save data directly to the DB during test set up, then run the tests. `pytest-django` also has utilities that reset the DB before each test, so life was good. This was only possible because the tests were run in the same context as the service; I think similar hacks would not work so well with, say, Java's `private`/`protected` internal classes.

## Test data, specifically
Enough academics, how do we solve this problem? In this test setup, the tests run in the `poetry` python environment, while the services are run in docker containers. Since I'm already using it extensively to orchestrate the services being tested, my solution was to use `docker compose run` once again to run python code within the `django` container that sets up the data.

To be clear, this is quite an ugly hack. The injected commands are strings containing python code that uses the Django ORM models, which are then passed to the [`shell_plus`](https://django-extensions.readthedocs.io/en/latest/shell_plus.html) `django-extensions` command, which runs within the docker container using `docker compose run`, which is invoked using `subprocess`. A daisy chain of tools like that is fragile, and debugging it requires intimate knowledge of every component. Performance is also quite bad; every invocation requires starting and stopping a container, which is an expensive operation.

Fortunately for me, the end result is actually expressed fairly concisely, so using it isn't too awful as long as it works. The whole point of the `goban-server` specification is that it shouldn't change much, and I'm the only developer, so I'm not too concerned about unexpected schema changes or merge conflicts.

## Alternatives
So far I have implemented aproximately half a test, just enough to verify that all the fixtures work as intended. It's possible that as more tests are layered on top, the performance penalties become more and more unnacceptable.

In that event, my plan is to include a dependency on `goban-server-django` in `goban-server-test`, wire it up to the test DB, and write helper functions that use the Django models to create test data and reset the database. This would eliminate the overhead of constantly invoking `docker compose`, and wouldn't be any less coupled than the current solution. The only reason I didn't do this first is because configuring a separate django app is somewhat involved, and because using `docker compose` was only about 10 lines of code.

## Next steps
As previously mentioned, I haven't written any real tests yet. The only test so far sets up a user with a known password and verifies that the response to `/api/login/` is 200. This is enough to exercise all of the existing lifecycle and test data fixtures, and it passes against both `django` and `fastapi`.

The next step is obviously to write more tests. The rest of the authentication tests can more or less be cribbed from the existing unit tests. The actual game logic stuff can probably be copied from `goban-server-django` as well.

Once there is a thorough test suite, it should be easier to test compliance when implementing the core business logic in `goban-server-fastapi`. It's possible to use the local file system as a git remote, and it's possible to manually adjust the HEAD of submodules, so testing against local changes should be easy.