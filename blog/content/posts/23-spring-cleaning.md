---
title: "Spring Cleaning"
date: 2023-03-16T20:26:57-04:00
type: post
slug: "23-spring-cleaning"
tags:
  - docker
  - github
---

With the release of the Haskell implementation, I took some time to mop up some of the mess I've made while trying to maintain momentum.

## Selecting implementation
Now that there are three implementations to choose from, I decided it was time to expose that functionality. I added a dropdown to the web app so that you can select which implementation is used for API requests. The Tailwind CSS library I'm using for components doesn't provide a very opinionated style for the dropdown, so it looks a little janky in mobile mode, but it works OK.

Having the dropdown revealed the fact that the Django implementation has actually been completely broken for a long time. When adding FastAPI to the nginx configuration, I also rearranged my nginx infrastructure so that my public facing nginx handled SSL termination and passed requests to the go-recordkeeper nginx, which distributed requests to the correct API based on a URL prefix (`/django/api/...`, `/fastapi/api/...`, etc.). The first nginx instance wasn't passing the `Host` header correctly, and Django was justifiably discarding any requests as their `Host` header did not match.

Even after that was fixed, the admin page was still broken. It turns out that when adding the implementation URL prefix, I neglected to configure Django correctly to include the prefix when generating URLs. I had to set [FORCE_SCRIPT_NAME](https://docs.djangoproject.com/en/4.1/ref/settings/#force-script-name) in the Django settings to fix it, which was not an easy piece of documentation to find.

After fixing up Django, all three implementations seem to be working fine!

Well, mostly fine. All the implementations work independently of each other, but there seems to be some interoperability errors when switching between them. Sometimes the session token from one implementation is not valid in another, causing the user to be logged out. Sadly the fix will probably require modifying some Haskell code, and I don't have the will to deal with redeploying it right now, so that's going to go into the backlog.

## GitHub Actions
Next I slapped together some CI with GitHub Actions. The value of CI is to provide immediate and obvious sanity checks for essential functionality, usually on PRs, so for me, it is not particularly valuable. I am a one-man show, so I've been committing directly to master. It also probably would have been more valuable while I was actively changing and improving things, but hey, no time like the present.

Ultimately, I pulled the trigger because I wanted some cool badges in the main README to prove that everything in an implementation was above board. Turns out it wasn't, so it's already useful!

### Pain and suffering
GitHub Actions, like every other CI/CD tool I've ever used, is obnoxious to set up. It's designed to run when things change in your repository, so the only way to test it is to commit something. It's like writing a Makefile, a bash script, and a docker compose file all at the same time, with no way to test it locally.

Still, once it's working it's pretty great.

I ended up having a workflow per implementation. Each workflow does linting (except Haskell, since I didn't set up anything but neovim autoformatting), unit testing, and then integration testing. Linting and unit testing was all reasonably fast and cacheable. Sadly, integration testing was not.

For the forgetful, the integration tests use a docker-compose file to spin up/down the services being tested and the postgres DB they interact with. This is great for compartmentalization, but means that images for each service need to be available. I set up the `docker-compose.yml` file to point directly to the `Dockefile`s for each implementation, because it makes it much easier to rebuild them in development.

Sadly, in CI, it takes a while to build the Python/FastAPI images, and it takes 15 minutes to build the Haskell image. Also, there's apparently no convenient way to cache the docker build process, at least, not without completely changing how I do the builds in development.

For now, I'm willing to just burn up my CI minutes, since I won't be touching those implementations very often. However, it would be nice to resolve all these docker build woes I have been having.

### Docker build plan
One approach would be to use a container registry to store the built docker images and use those images instead of building them from scratch every time. Ideally, those images would also use `buildx` multiarch support so that they can also be used on the Raspberry Pis, which have ARM processors. Builds would happen in CI and would be uploaded to the registry upon success.

Hmm, building multiarch actually requires QEMU to emulate the different processor architecture, so maybe that's not really possible in CI. If multiarch builds need to happen on my development machine, could they maybe also be used in CI? It would be very challenging to guarantee that the image in the registry corresponds to the changes that triggered the workflow. Hmm.

I think there's a way to specify both a `build` context and an `image` for a docker compose service, so running `docker compose build` would still build and use the local code for integration testing locally.

Thoughts for later.

## Tidying up 
Anyway, after all the CI steps were working they revealed that the Django tests have been broken for ages, and Django and FastAPI both needed linting (Haskell was pristine, thankfully). The Django test errors were due to renaming some fields in the JWT; I had updated the application code, but not the tests. This is exactly what CI is meant to catch, so I am grateful that it is now there to advocate for the "finished" implementations.

## Next steps
I've finally started filing GitHub issues to track all my TODOs. My naive hope was that I would simply deal with them as they came up, which has proven impossible as the number of moving parts increase. I suppose I should have predicted it.

Anyway, there are a few maintenance tickets in the backlog now that need to be chewed through eventually:

* The implementation switching bug mentioned above
* Docker container registry/multiarch builds
* Cosmetic restyling the UI a bit, it's kinda ugly, not great on mobile (embarrasing)
* Cleaning up the READMEs. I'd like the repository to be a testament to my incredible skills, and the only part anyone might ever plausibly look at is the READMEs, so they'd better be 🔥

The thing I'm actually interested in doing is the Rust implementation, since that's like, actual code. We'll see what happens first.
