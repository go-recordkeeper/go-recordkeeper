---
title: "Monorepo"
date: 2022-11-14T20:36:14-05:00
type: post
slug: "10-monorepo"
tags:
  - django
  - fastapi
  - nginx
---

I bit the bullet and coallesced all the repositories into a single monorepo: [go-recordkeeper](https://github.com/go-recordkeeper/go-recordkeeper). This looks less impressive, in the GitHub organization view, but is generally much nicer to work with. I left the old repositories there just to avoid breaking all those links, so I guess the trail of corpses is still imposing.

RIP git submodules 😭

# Monorepofication

The conversion process was rather lengthy and quite involved. I hope that my case was abnormally difficult because I had a lot of submodules that needed conversion into relative paths, but I cringe to think how much harder it would have been with more coworkers, possibly committing work at the same time. As it is, I'm the only contributor (and also the only user of the app), so the headaches belong solely to me.

It's late and I'm tired, so here's a quick summary of the speed bumps I hit:

* I had to write (steal) a script to copy the git history of all the coalescing repositories. git has a neat command `format-patch` that with the right incantations dumps out the entire repo into a file as a series of patches. By editing that patch stream with `sed` to prefix all the file paths with their new home in the monorepo, you basically edit history so that all of the changes made were, according to git, made in the monorepo. Credit to [Tim Holzerr](https://medium.com/@TimHolzherr/how-to-move-your-git-repository-into-a-monorepo-without-loosing-its-history-9b9d2da27155) for that
* I was using a submodule to import the Hugo theme used for the blog ([gokarna](https://github.com/526avijitgupta/gokarna), nice theme, maintainers have been very responsive). This exploded immediately when it was moved to a different path in the monorepo, so I had to exterminate it and add it afresh.
* To keep the vue code vuey and the pure JS code JSy, I originally split the web app code and the pure JS canvas code that renders the game board into two different repositories. I used an npm git dependency rather than export an npm package to import the board code into the vue code, but now in the monorepo, I can use a relative import. Unfortunately this [broke the build](https://rollupjs.org/guide/en/#error-name-is-not-exported-by-module) because JS is bad. I eventually resolved it by changing the board code to export as ES2022 JS instead of CommonJS. It's less portable, but ¯\\\_(ツ)\_/¯
* All the deploy build scripts needing updating, but that's only to be expected.
* The tests basically just worked after find/replacing submodules with relative paths, which was nice.
* I wrote scripts to backup/restore the database so that the old data could be uploaded to the new monorepo deployment. Thankfully the schema is unchanged, so everything went off without a hitch.
* Documentation needed to be updated everywhere. I think I got all the egregious stuff, but you can always document better.

The tests passed and the deployment went smoothly, so naturally it's time to keep working!

# Deploying FastAPI

Everything is in one easy place now, so how hard could deploying FastAPI really be?

It's pretty hard.

Most of the difficulty was in testing the deployment locally. I use [nginx-proxy](https://github.com/nginx-proxy/nginx-proxy) to abstract away the nitty gritty nginx config writing (which I could have done manually), but mostly to generate Let's Encrypt SSL certificates automatically (which I really did not want to do manually). This makes deploying in production very easy, since SSL just pops out for free. However, it makes developing locally hard, since your development machine probably doesn't have it's own domain.

I, foolishly, simply redirected all my incoming network traffic to my laptop (instead of my Raspberry Pi which normally runs the server), which made `go.chiquit.ooo` resolve to my laptop, allowing me to essentially test in production (without production data, of course). In retrospect, I should have just commented out the letsencrypt stuff from the `docker-compose.yml`. Oh well, live and learn.

I ended up hosting the Django instance at [https://go.chiquit.ooo/django/](https://go.chiquit.ooo/django/) and FastAPI at [https://go.chiquit.ooo/fastapi/](https://go.chiquit.ooo/fastapi/). Since there is no longer a single [https://go.chiquit.ooo/api/](https://go.chiquit.ooo/api/), I simply pointed all web client traffic to FastAPI; it deserves some time in the sun. I couldn't find a simple way to enable load balancing with `nginx-proxy`, so I will either have to hit my harder on that problem or just choose API providers randomly client side. A problem for another day.

The last bump was the [OpenAPI docs](https://go.chiquit.ooo/fastapi/docs). FastAPI has some really nice OpenAPI integration (IMHO much nicer than [drf-yasg](https://drf-yasg.readthedocs.io/en/stable/)), so I figured it would be nice to have the OpenAPI docs page available somewhere. Sadly, the page pulls the OpenAPI data from `/openapi.json`, and that isn't configurable, so I had to add a custom nginx redirect to `/fastapi/openapi.json`. It feels dirty, but I don't know how else to do it.

Now that I'm looking at it again I found [these docs](https://drf-yasg.readthedocs.io/en/stable/) that describe how to disable the default `/doc/` and `/redoc/` and serve your own with custom templatization. Seems a lot more involved than my ugly proxy thing. Ah well.

# Next steps
1. Pat myself on the back. I've got a monorepo, two working implementations, and even some docs. That's pretty good. 
1. Do some FastAPI code review. Django is officially the source of truth for the DB because of its stellar ORM, but I think FastAPI is going to be what any subsequent implementations are going to resemble (it's vertically sliced, after all). It should look as good as possible, and more importantly be as copiable as possible.
1. Get to work on alternative implementations. I want to start with Haskell. I have a friend who might be interested in the Rust implementation, so I might also take this as an opportunity to practice project management and code review.
