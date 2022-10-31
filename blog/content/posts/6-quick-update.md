---
title: "Quick Update"
date: 2022-10-31T19:09:06-04:00
type: post
slug: "6-quick-update"
tags:
  - pytest
showTableOfContents: false
---

I just finished a work session, so a post feels obligatory, even though there's nothing substantial to talk about.

I finished the authentication and record CRUD integration tests. Everything passes for `django`, and all the auth tests pass for `fastapi` (records are not yet implemented). Writing the tests uncovered a few small issues:

* `django` was accepting arbitrary integer board sizes. I fixed this to use a [choices](https://docs.djangoproject.com/en/4.1/ref/models/fields/#choices) of 9x9, 13x13, or 19x19.
* `django` was returning `201 CREATED` instead of `200 OK` from record update requests. This was corrected and the unit test corrected.
* `fastapi` was returning `401 UNAUTHORIZED` instead of `403 FORBIDDEN` when the authentication token was missing/invalid. This is hypothetically acceptable, but it isn't compliant with the `django` implementation, and `401` also technically requires setting the `WWW-Authenticate` header to inform the client of the authentication method, which I wasn't doing.

On an unrelated note, I also updated [goban-deploy](https://github.com/go-recordkeeper/goban-deploy) to use git submodules instead of the script I wrote to pull the requisite repositories. Submodules simply didn't occur to me when I was setting it up, they are the perfect solution.

The next steps are to:
* whip up some tests for playing moves
* finish implementing `fastapi`
* deploy `fastapi`, hopefully load balanced
* do some more interesting implementations
