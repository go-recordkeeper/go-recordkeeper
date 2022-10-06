---
title: "FastAPI Testing"
date: 2022-10-06T16:43:23-04:00
type: post
slug: "4-fastapi-testing"
tags:
  - fastapi
  - pytest
showTableOfContents: true
---

As previously mentioned, the next step is to add [pytest](https://docs.pytest.org/) for unit testing. Mission accomplished.

This was pretty run of the mill testing stuff so I didn't really learn anything, but I'm trying to write something for every unit of work I do, so I shall expound on my testing philosophies instead.

## Unit tests: just say no

For the record, I love tests. I worked for several years doing automated browser testing and enjoyed it. Testing is crucial and no application is complete without an automated test suite.

Tests prove the correctness of your system, and having correct systems is obviously good. However, tests are code just like the rest of your application, and code is inherently a liability. If you exhaustively test every function, method, and component in your application, then making any change of any kind means a cascade of associated tests that need to be tweaked and refactored. In some exhaustively tested applications, I've found that PRs tend to have more changes to the test code than to the application code.

There's an oft cited concept in test automation called the [Test Pyramid](https://martinfowler.com/articles/practical-test-pyramid.html) which says that you should have lots of very fast unit tests, fewer system/integration tests, and some token number of UI or full stack tests. I totally agree that more fast tests and fewer slow tests is a great idea, but I'm actually not a fan of exhaustive unit testing (🔥 hot take 🔥).

### Don't test simple code

Unit tests are most valuable when you have easily isolated units of complicated code. If the code is simple, it's not necessarily worth it to test. For example, this is a very important yet very simple function that generates the authentication token when a user logs in:
```python
def generate_token(user_id):
    return jwt.encode(
        {"id": user_id, "exp": datetime.now(tz=timezone.utc) + timedelta(days=1)},
        key=SECRET_KEY,
        algorithm="HS256",
    )
```
What is there to test? You could verify that the output is shaped like a JWT, or that it decodes correctly with the correct `SECRET_KEY` and doesn't decode without it, but [pyjwt](https://pyjwt.readthedocs.io/en/latest/) has definitely already tested those things more exhaustively than I ever could. Besides, if any of that doesn't work, the tests for the login endpoint will also break.
The only thing I can possibly think to test is that I did the date math correctly to set the token expiration. Still, it would be more valuable to write a login endpoint test that uses an expired token so that the error handling around the expiration stuff is also tested.

Ultimately, tests exist to catch and prevent bugs. If a test is so inane that it would never conceivably stop a bug, it shouldn't exist. Focus on testing the complicated code that might be actually be concealing bugs.

## Mocks: just say no

Well, what about unit testing complicated code that has dependencies? Conventional wisdom is to use [mocks](https://en.wikipedia.org/wiki/Mock_object) to simulate the behavior of downstream dependencies. This sounds like a great idea if you have code that, for example, calls a database or an external API or does some kind of expensive system call. Instead of doing the thing for real, you just record the expected behavior of the dependency in the test, call the code being tested, and verify that it does the right thing when the dependency behaves as you told it to. Everything runs super fast since nothing external is actually called, and you have the power to test whatever wonky dependency behavior you want.

I think mocks are awful and should be avoid wherever possible. Managing test data is, in my humble opinion, already one of the hardest problems in automated testing. Now instead of black box tests where you only need to concern yourself with arguments and return values, mocks require you to also hardcode the inputs and outputs from everything your unit is invoking. Encapsulation is completely broken since you need to know exactly what the test code is doing in order to feed it the correct mocks. Your mocks are necessarily tied to the API of your dependencies, which means your test code is too. Worst of all, you don't even know if your code actually works when actually plugged in to all its dependencies because you are only testing it against a fictionalized idealization.

So how are you supposed to test code that is complicated enough to have bugs but also has external dependencies? Well, that sounds like some 🍝 to me. Refactor it so you have some complicated code with no dependencies, and some simple code with dependencies. Write unit tests for the complicated code and integration tests for the dependency-laden code. Just don't use mocks.

I'm sure someone somewhere is writing an application that interacts with a credit card processing API or something and absolutely, positively must mock it, but I have yet to see that use case. [Stripe](https://stripe.com/) has a [test server](https://stripe.com/docs/testing); they should probably use that instead.

Lots of hot takes here, don't @ me, I don't have twitter

## Integration tests

The definitions of "unit tests", "integration tests", and "system tests" are all fuzzy and highly debatable. When I think of unit tests, I think of tests for pure code; no extra infrastructure or network requests. When I think of integration tests, I think of tests for code that needs to communicate with external services. You should obviously unit test everything you can unit test, but when building a web service, most of the "interesting" behavior will be related to its integration with other services. Therefore, I recommmend emphasizing integration tests appropriately.

## My testing environment

That's a lot of words to say that I haven't written any unit tests. I'm writing a web application, which means a lot of code is interacting with either FastAPI or SQLAlchemy. If I were to try to test any of that code in isolation, I would need to mock the database, and I ain't doin' that. The only actual business logic I have is the password hashing and JWT stuff. As long as the implementation as a whole behaves identically to the reference implementation, there's no real need to test code I copy/pasted.

Instead of unit tests, I am writing integration tests. All tests require that a PostgreSQL DB be available. Before every test, the relevant tables in the DB are wiped so that every test starts from a blank slate (pytest-django has [something similar](https://pytest-django.readthedocs.io/en/latest/helpers.html#pytest-mark-django-db-request-database-access), so I made my own, much less cool version). I wrote some Pytest [fixtures](https://docs.pytest.org/en/7.1.x/explanation/fixtures.html#about-f0ixtures) that generate a user and store it in the database for tests to access (fixtures are amazing, I highly recommend reading up on them if you're not familiar).

The downside of this is that tests are relatively slow since actual SQL queries are being made. Right now I have 17 tests for registering and logging in, which run in about 1.6 seconds. In a larger app with more tests, it might easily take multiple minutes to run the whole test suite. In my opinion, I think that's an acceptable price to pay for not having mocks. When working on a specific part of an appp, I run the tests for that specific feature. I only run the whole suite before committing (or in CI), at which point I'm not in such a hurry to keep the flow state going.

## Next steps

The next step is to leverage all this nifty testing infrastructure to add the real business logic of the app. One part of that is refactoring the code that identifies captured groups, which is both very complicated and completely free of dependencies, and therefore unit testable! I actually already have [unit tests](https://github.com/go-recordkeeper/goban-server-django/blob/main/record/test/test_board_replay.py) for it from goban-server-django, which should make refactoring much easier.
