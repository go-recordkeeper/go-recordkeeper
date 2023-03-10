---
title: "Finishing Haskell Deployment"
date: 2023-03-09T21:02:10-05:00
type: post
slug: "22-finishing-haskell-deployment"
tags:
  - haskell
  - docker
showTableOfContents: true
---

After a month of respite, I have finally mustered the courage to get the Haskell implementation deployed and achieve some closure.

## Building Haskell

My first step was to simply retry the build and hope it just works. Not only did the Haskell build continue to crash, but the FastAPI build also began failing. So that was swell.

### Making Pi from scratch

My next step was to switch to a different Raspberry Pi, do a clean reinstall, and start from scratch. I have two Raspberry Pi 3's and one Raspberry Pi 4, so I decided to upgrade to the Pi 4. Sadly I could not locate my [Type D](https://en.wikipedia.org/wiki/HDMI#Connectors) adapter, so I had to go through the installation process on a Pi 3 using an old monitor and an HDMI-VGA adapter. Thankfully the Pi 4 booted after switching out the SD card.

With a nice fresh installation, I kicked off a build and let it spin for a few hours. When I came back to it, it had failed due to a network error. My internet is spotty and spontaneously drops for a few minutes a couple of times a day, and with the extremely long Haskell build times I could expect this was not really acceptable.

### Caching stack builds

To get around this, I discovered that `stack` caches its work in the `~/.stack` and `.stack-work` directories. Caching these directories during the build process allows some degree of incremental progress:

```
RUN --mount=type=cache,target=/root/.stack \
  --mount=type=cache,target=/opt/haskell/.stack-work \
  stack build --only-dependencies
```

After some aborted attempts, it seems that progress is only saved per dependency listed in `package.yaml`. Once a dependency is built, it doesn't need to be repeated on subsequent builds, but aborting a build means the current dependency will need to be restarted. This level of granularity was especially problematic for the [`lens`](https://hackage.haskell.org/package/lens) package, which I estimate took 18 hours to compile. Thankfully it succeeded the second try.

### Memory management

Even after getting some traction with this approach, the build was now reliably crashing around the same point after chewing for several hours. Some sleuthing revealed that I, in my infinite frugal wisdom, purchased the 1 GB RAM Raspberry Pi 4. In addition to the ~1.5 GB of swap, `stack build` was easily exceeding 2.5 GB of memory (wow). 

My first guess was to single thread the build process. I noticed four different `ghc` processes in `top` while the build was going, which matched the number of cores. I also noticed that the build logs generally alternated between two or three packages, interspersing modules from multiple packages. Presumably running four builds at once would require four times as much memory.

After explicitly limiting the build to a single thread, I noticed no change in behavior. It's possible that I just did it wrong somehow, but documentation suggests that one thread is actually the default, and you need to explicitly opt in to multithreaded builds. No luck there.

The solution was to just add a new 4 GB swap file. Utilization topped out around 4 GB, so this was a good guess. `kswapd0` was also going crazy the entire time, and I cringe to think of the damage I did to my SD card, but it never actually crashed.

### Waiting...

I waited a long time. With frequent restarts, interruptions, and tinkering, I was babysitting the Haskell build for about 36 hours.

But it finished!

At last, I have a functioning ARM Haskell image of the application.

Sadly, I fear that I will never be able to update it on pain of another tragic build marathon, but at least it's over.

## Building FastAPI

I'm still not sure what broke here, tbh. When running the `poetry install` step of the build, any number of wheels would report an "invalid hash" and crash the build. Most often, it was `uvloop`, but sometimes `uvloop` would work and frequently other packages would fail. The non-matching hash value would change every time, as well. My working theory is that downloads were having a bit flipped or something and poetry was simply not handling that intelligently. I don't know how that would happen short of downloading over UDP (!!!), but there we are.

I tried upgrading the poetry version, with no success.

I tried enabling the poetry build cache in the same manner as the Haskell build cache, in the hopes that the good packages would be cached, and the bad ones would be retried next time. This actually made the problem worse; the bad packages were cached, and the exact same errors were spat out every build.

Ultimately, I just retried the build a bunch of times, and it eventually succeeded. No closure there, I guess. Hopefully I never need to update FastAPI either.

## Deploying!

### Failure to deploy

As previously mentioned, I switched Pi's while debugging. Unfortunately, during all the SD card juggling, the SD card formerly hosting the application stopped booting. This was very concerning, as it also had the database with all of my games on it.

Fortunately, it still mounted OK on my laptop, so I was able to recover the docker volumes. They are conveniently stored under `/var/lib/docker/volumes/{volume id}/_data/`, so it was fairly trivial to just replace the contents of the empty DB volume on the new Pi with the old DB contents. I would have preferred doing a proper SQL dump/load, but all's well that end's well.

### Success to deploy!

It worked! At least, the container ran. I wrapped up the final deployment steps about 30 minutes before heading out to play a game which I wanted to record, so I didn't tempt the fates by updating the front end to use the Haskell API. That is a problem for next week.

## Next steps

Nothing Haskell related, that's for gosh darn sure. 

* That said I do need to test that it works. Turns out load balancing doesn't work because the different implementations handle CSRF differently, so just slapping together an nginx config isn't an option. I've been thinking of putting in some kind of switcher to choose which backend you would like to use. It would only really be useful to me, but it would be neat.
* Move the DB in prod. I have a postgres DB running on a separate server, I just need to migrate the data and aim the production deployment at it. It's not hard, and after being burned I really need to learn my lesson.
* Do a new implementation! Now that I've finished all this scut work perhaps some green field development would spark joy. Rust, Java, F#, the possibilities are endless.
