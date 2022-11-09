---
title: "Vertical Slice Architecture"
date: 2022-11-09T14:22:45-05:00
type: post
slug: "7-vertical-slice-architecture"
tags:
  - fastapi
showTableOfContents: false
---

I've been flipping through some [NDC Conference](https://www.youtube.com/c/NDCConferences) recordings, and one of the [talks](https://www.youtube.com/watch?v=SUiWfhAhgQw) caught my eye (also a considerably more succinct [blog post](https://jimmybogard.com/vertical-slice-architecture/), if you don't have an hour to kill).

For context, the traditional .NET web application is designed in layers (the "onion model", apparently?), with the database feeding up into the repository into the service into the UI. All of your SQL queries live in one place, which is separate from where your business logic lives, which is separate from your REST API, which is separate from your UI components. I've never done .NET before, but this is exactly how I learned to write web applications in Java and Python.

The pitch of the talk is that your application should be sliced vertically instead of horizontally. This creates a much tighter vertical coupling between all the code necessary for a single feature, all the way from the database to the UI, and decouples that feature from the other features. Adding or updating features is easier for developers, since all the code changes are contained in one slice, and modifying one feature is by design not going to have accidental side affects on any others. [Jimmy Bogard](https://jimmybogard.com/vertical-slice-architecture/) is considerably more eloquent if you'd like more details.

I've always sliced my apps horizontally, so the thought was intriguing. The talk was very much .NET oriented; I wasn't familiar with a lot of the jargon used (repository? but not git??) and described solutions involved some .NET specific packages and architectures. Still, the point about coupling was good, so I decided the FastAPI implementation would be a good chance to try out this new style.

I had only finished the user auth bit of the app, so I decided to slap some more code in there first so I would have something worth slicing. I picked the list records endpoint, which also required some database helpers, the record factory test fixtures, and a couple of tests. Everything was pretty uneventful, nothing unexpected. The FastAPI list records integration test passes, so mission accomplished.

The next step was to move all of the code into it's new vertically sliced organization. Big refactors like this are not really linear processs, so I'll just list the hurdles I hit.

Firstly, it's obviously impossible to slice everything. FastAPI apps are usually built like this:
```python
app = FastAPI(...)

@app.get("/my/endpoint/")
def my_endpoint_handler(...):
    ...
```
Every endpoint has a dependency on the one `app` instance, so every slice needs to import it from some central [`rest.py`](https://github.com/go-recordkeeper/goban-server-fastapi/blob/68564505d4073e211cf1aa46e9300837e36926f2/goban_server_fastapi/rest.py) (as I named mine).

Similarly, SQLAlchemy has a central registry for all ORM models, and is designed to have a single shared client instance for the whole app. I used [`db.py`](https://github.com/go-recordkeeper/goban-server-fastapi/blob/68564505d4073e211cf1aa46e9300837e36926f2/goban_server_fastapi/db.py) to store this code, and used FastAPI dependency injection to provide a special `DbClient` instance.

The major hurdle was dealing with authentication. The idiomatic way to do this in FastAPI is [dependency injection](https://fastapi.tiangolo.com/tutorial/security/get-current-user/). In my implementation, every protected endpoint specifies a dependency on `jwt_user`, which checks the `Authorization` header in the request context for a valid token. This logic obviously depends on the `User` ORM model, and is at least logically adjacent to login/registration, so it makes sense to live next to some kind of "auth" vertical slice. But, every other slice is going to need authorized endpoints, and inter-slice dependencies defeat the whole purpose.

I couldn't find a clean way to resolve this, so for now the "record" slice has a dependency on the "auth" slice to get the `jwt_user`. I considered importing via the global `rest.py` instead to avoid the appearance of the dependency, but this creates a dependency cycle. It's resolvable thanks to Python's runtime import system, and it would hypothetically be nice to just grab everything endpoint related from `rest.py`, but I think the fragility of having a dependency cycle outweighs any benefits.

So far, I'm not entirely sold, but I haven't scaled up enough to the point where vertical slices would prove their worth. With only one record endpoint, it is nice having everything in one file. Time will tell once the rest of the endpoints are in place.