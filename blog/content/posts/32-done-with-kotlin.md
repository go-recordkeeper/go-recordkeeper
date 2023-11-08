---
title: "Done With Kotlin"
date: 2023-11-07T17:48:59-05:00
type: post
slug: "32-done-with-kotlin"
tags:
  - kotlin
  - spring
---

Done and dusted. I confess that using Spring was not very inspiring. I did not have very fond memories of it, and now that seems justified. Kotlin was pretty neat though, considering that it's constrained by running on the JVM.

## Spring
In fairness, Spring is very mature and has decades of layers and growth. It is capable of doing everything, and I assume that if I had grown along with it and had some knowledge of its expectations, I would have had a nicer time. For what I was trying to accomplish, Spring was not the right tool.

Even with that taken into account, using Spring was not a pleasant developer experience. Javadoc has a tendency to simply describe functions (`Object getAttribute(String name, int scope) - Return the value for the scoped attribute of the given name, if any.`), which is useful when you know what you are looking for and need to know what it's called. Javadoc is less useful when you want to find out why something is the way it is, or how a particular class fits in to the larger architecture, or even common usage examples for the code so loquaciously described. 

Higher level documentation is a much more difficult thing to do well, and indeed I found that Spring does not do a very good job. There are a vast number of official and unofficial quickstart guides for setting up your first Spring project in 10 minutes flat, and the [official reference](https://docs.spring.io/spring-boot/docs/current/reference/html/) is a dense slug of information about things Spring can do, but I could find precious little information about how things like the request lifecycle and server configuration actually worked. Perhaps it was in there somewhere, but I was dazzled by such delightful nuggets as [The ServletWebServerApplicationContext](https://docs.spring.io/spring-boot/docs/current/reference/html/web.html#web.servlet.embedded-container.application-context):

> 1.3.3. The ServletWebServerApplicationContext
> 
> Under the hood, Spring Boot uses a different type of `ApplicationContext` for embedded servlet container support. The `ServletWebServerApplicationContext` is a special type of `WebApplicationContext` that bootstraps itself by searching for a single `ServletWebServerFactory` bean. Usually a `TomcatServletWebServerFactory`, `JettyServletWebServerFactory`, or `UndertowServletWebServerFactory` has been auto-configured.

Perfectly true and valid information, but not useful without an awareness of what an `ApplicationContext` is, why embedded servlet containers need to be supported, or what the `ServletWebServerFactory` bean might mean. None of this context is available in the surrounding documentation, as far as I can tell.

There is also a wealth of third party documentation and tutorials that are relentlessly SEO'd, most notoriously [Baeldung](https://www.baeldung.com/). They frequently have usage examples that I could not find in the official docs, but again do not offer much in the way of contextual information about how Spring works.

Overall, the Spring documentation seems to assume some fundamental awareness of the Java EE context that Spring swims in, while the guides and tutorials all assume that you are a Junior level intern who just needs a web server running ASAP.

## Kotlin
Kotlin is pretty cool. After my forays into the rest of the programming world, Java does not seem very cool, but Kotlin does its best to ameliorate the worst parts of Java.

* Some of the more onerous Object Oriented rules and regulations are done away with. Files are no longer one-to-one with classes, `this` is now implicit when referencing members, and lambdas have sane types.

* Null pointers are now strictly opt-in. If a variable could be null, it must be flagged with a `?`. Any potential `NullPointerException` must be checked on pain of a compile error, which is much better than a sneaky runtime exception. I confess that I used the [Optional](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html) class from Java, as it is more reminiscent of Rust's [Option](https://doc.rust-lang.org/std/option/index.html) or Haskell's [Maybe](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Maybe.html). Still, having confidence that there are no NPE's hiding anywhere is pleasant.

* Lambdas can be defined extremely concisely using a simple `{ }` block. Consider this map expression which increments every element in a sequence:
```kotlin
sequence.map { it + 1 }
```
`it` is a hardcoded parameter that I assume is defined in the `map` function, which is a thing I've never seen before. It's kinda nice, honestly.

* Classes defined like functions, which is pretty neato. Check out the `GoBoard` implementation:
```kotlin
class GoBoard(size: Int) {
  val size = size
  val stones = HashMap<Position, Color>()
  val moves = ArrayList<Move>()
  // ...
  fun adjacents(pos: Position) = sequence {
    if (pos.x > 0) yield(fromCoordinates(pos.x - 1, pos.y))
    if (pos.x < size - 1) yield(fromCoordinates(pos.x + 1, pos.y))
    if (pos.y > 0) yield(fromCoordinates(pos.x, pos.y - 1))
    if (pos.y < size - 1) yield(fromCoordinates(pos.x, pos.y + 1))
  }
  // ...
}

```

The primary (and in this case, only) constructor takes a `size: Int`. Anything declared in the constructor is considered to be fields (`size`, `stones`, and `moves`) or methods (`adjacents`). I also declare `adjacents` using `=` rather than the traditional `fun functionName() {}` syntax because `sequence` returns a generator function.

Overally, Kotlin was surprisingly ergonomic considering all the baggage of the JVM that it has to drag along. I quite enjoyed it.

## My experience
This is where I'd put the chronicles of what I did and what I learned, but quite frankly it's late and I'm tired, so I think I'm going to skip it. Rest assured that all this work was non-trivial, it took some time. I mostly copied stuff from the Rust implementation and translated it into Kotlin. I made plenty of mistakes, I made some minor improvements to the test infrastructure, I squashed some bugs. None of it was particularly interesting though, so I relegate the tale to the dusty halls of the git log.

Kotlin is finished, which is the important takeaway.

## Next steps
I think I'm done with new implementations for a bit. I've got the rhythm pretty much down, so it's mostly just doing the same stuff I've done 6 times before, but with a marginally different syntax. Until I find a genuinely novel language or tech stack, I think I'm happy for now.

There are a few API changes that I'd like to address, like error handling, stone/move serialization, and CORS. Maybe I'll do those if I ever get the urge to exercise 5 languages at once.

I could also do some performance profiling and load testing. I've basically completely ignored that because I'm still the only registered user, but it's good practice to consider those things.
