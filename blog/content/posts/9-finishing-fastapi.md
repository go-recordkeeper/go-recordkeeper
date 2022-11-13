---
title: "Finishing Fastapi"
date: 2022-11-13T18:12:13-05:00
type: post
slug: "9-finishing-fastapi"
tags:
  - fastapi
  - pytest
showTableOfContents: true
---

I finished the FastAPI implementation, and the integration test suite now at least partially covers every endpoint. Considering the amount of work, there is surprisingly little to say about it.

The vertical slice architecture was nice to work with. I ended up creating a separate file for every record related endpoint, which basically just contained one function. It's very legible, and maps directly to the corresponding test files. In fact, I'd like to put the tests in the endpoint files directly, but pytest needs a `conftest.py` so this is difficult.

The integration tests did uncover a bug with rendering passes, and some status codes that didn't match the Django implementation. I'm going to hash those out later.

### Next steps
I'm going to bite the bullet and convert into a monorepo. I initially used separate repos because it looks cooler in git, but that's not really worth the inconvenience of coordinating and synchronizing all the implementations. Tragically, that means no more git submodules 💀

After that, I'm going to get started on the Haskell implementation.