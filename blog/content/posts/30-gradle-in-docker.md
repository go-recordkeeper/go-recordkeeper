---
title: "Gradle in Docker"
date: 2023-10-15T14:18:59-04:00
type: post
slug: "30-gradle-in-docker"
tags:
  - docker
  - kotlin
  - gradle
---

The first step in starting a new implementation after the Hello World is to get it Hello World'ing in a docker image. After copy/pasting a boilerplate `docker-compose.yml`, we get integration tests and a local deployment DB for free.

Writing Dockerfiles that technically work is pretty trivial; you just `COPY` the entire project directory in, run your build tool (`gradle` in this case), and there you go. However, this is problematic in practice for compiled languages (like Kotlin), since the build is generally very expensive and time-consuming to run from scratch every time. Since the image needs to be rebuilt every time we want to rerun an integration test, it's very important to this project that the `docker compose build` be snappy.

## Introducing Gradle
It's been a long time since I've used Java or anything in the Java ecosystem, so all of my JVM build tooling experience is with [Maven](https://maven.apache.org/) (in Eclipse lol). I don't miss working with XML files, so I decided to give [Gradle](https://gradle.org/) a shot.

Gradle has, shall we say, a unique set of best practices for usage. The main entrypoint is a shell script `gradlew` (or `gradlew.bat`, if that's how you want to live your life) in the project root which is (ironically) a wrapper around an executable JAR, `./gradle/wrapper/gradle-wrapper.jar`. This Gradle Wrapper™ will automatically download the latest/pinned version of Gradle into `~/.gradle/`. The Wrapper then transparently proxies all invocations to this Gradle binary. This is cool and great for people who care deeply about maintaining multiple Gradle builds with multiple versions on the same machine, but really makes it a headache for me to Dockerize effectively.

### Jib
I am obligated to mention [Jib](https://github.com/GoogleContainerTools/jib) here. Jib seems to be the accepted tool for building Docker images instead of JAR files, which is basically exactly what I want. It plugs in nicely into Gradle and will automatically recognize and handle Spring applications properly, which is exactly my use case.

The only problem is that it is a Gradle plugin, so I would need to install and run Gradle as part of my global deployment process. I already have so many implementations that the only thing keeping everything sane is that all the build details can be abstracted away behind a `docker-compose build`. Even then, if it were possible to use a container registry, I would still probably prefer Jib. Sadly the alternative CPU architecture of my Raspberry Pis makes this more or less impossible.

## Docker build caching
I've probably mentioned the process of optimizing Docker builds in the past, but it's quite important and bears repeating. I encountered a lot of problems getting Gradle builds to run quickly and burnt several hours refining the process, so I'd might as well document it.

The primary use case I am optimizing for is incremental builds of minor source code changes. A common DX experience is to write a new feature, run an integration test, have it fail, make a small adjustment, rerun the test, and repeat ad nauseam. Because each integration test run requires a fresh build of the image, it needs to be fast. There are a different set of optimizations that might be practical in a different project to reduce overall build time, but I frankly don't care very much about that. 95% percent of my time is spent developing, so that's where my optimizations go.

The key to incremental build performance is spacing out your `COPY` statements in your `Dockerfile`. Whenever `docker build` is run, Docker will cache each build step and reuse that cache if nothing has changed. As soon as a file that was `COPY`'d into the build is modified, that invalidates the cache for the rest of the file, meaning every subsequent step needs to be rerun. Therefore, your least frequently changed files need to be copied over first, while your most frequently changed files (i.e., `src`) need to be copied last.

I wound up using three build phases in total:
* Install Gradle
```Dockerfile
COPY --link ./gradle /goban/gradle
COPY --link ./gradlew /goban/gradlew
WORKDIR /goban
RUN ./gradlew
```
  This copies over only the Gradle Wrapper and invokes it with no arguments, which triggers a fresh installation of Gradle. The installation isn't referenced explicitly, but it is living in this layer of the imaged and can be accessed by all subsequent invocations of `./gradlew` in the build process. The `gradlew` file should basically never be modified, so this step should basically only be run once. This step takes about a minute.
* Cache dependencies
```Dockerfile
COPY *.gradle.kts /goban
RUN ./gradlew --profile --build-cache --configuration-cache bootJar || echo "Failed build, as expected"
```
  This copies over the Gradle configuration files and attempts to run a build (`bootJar` means build a JAR file that runs the Spring Boot application). Because we haven't copied over the source yet, this build will fail, which we are OK with. All we really want it to do is resolve and download dependencies, which it conveniently does. Whenever we need to add or modify dependencies, we will modify `build.gradle.kts`, which will trigger this step to run again. This step takes about a minute and a half.
* Build the project
```Dockerfile
COPY --link ./src /goban/src
RUN ./gradlew --no-daemon --build-cache --configuration-cache bootJar
```
  This copies all of our project sources and runs the build. Thanks to our cached layers, this step only takes about 8 seconds.

## Next steps
`docker compose build` runs in ~11 seconds now, which is acceptable. I don't see any more configuration options available in Gradle, so that's probably the best I can do. The next step is to actually learn some Kotlin and start working on the auth endpoints.
