---
title: "Spring Suffering"
date: 2023-10-17T21:20:19-04:00
type: post
slug: "31-spring-suffering"
tags:
  - kotlin
  - spring
showTableOfContents: true
---

Spring sucks, and I hate it. It's late and I'm tired, but I'm trying to relate my sufferings now while they are still fresh.

# The Goal
My objective was to set up the `/api/login/` endpoint, which is always my first step when working on a new implementation. There are several foundational components that all need to work together to achieve this:

* Request handling (obviously), with parameters and serialization/deserialization
* Password hashing
* JWT key signing
* SQL querying
* Configuration from environment variables

## Request handling
This was pretty easy. I already had a working stub of Spring Boot application (basically just a single file with a `fun main(args: Array<String>)`), so I just needed to add a new file for the login endpoint:
```kotlin
@RestController
class LoginController {
  data class Request(val username: String, val password: String)

  @PostMapping("/api/login/")
  @ResponseBody
  fun login(@RequestBody request: Request): String {
    return "\"not a real JWT lmao\""
  }
}
```

This is all pretty straightforward, canonical Spring stuff. The `@RestController` annotation declares that this class contains REST endpoints. The `@PostMapping("/api/login/")` declares that this method handles `POST /api/login/` requests. `@RequestBody request: Request` declares that the request body shall be JSON deserialized using the declared `Request` type, and `@ResponseBody` declares that there shall be a response body (I'm serializing it myself because it's just a simple JSON string). Spring automatically scans the classpath and checks for those annotations, so we don't even need import or map any routes ourselves. Magical.

## Password hashing
Spring has a security crypto module which includes password hashing, so naturally I tried to use it. After getting everything wired up, I discovered that because Spring uses an extremely generic [`PasswordEncoder`](https://docs.spring.io/spring-security/site/docs/current/api/org/springframework/security/crypto/password/PasswordEncoder.html) interface, you are not allowed to specify a salt. Instead, Spring will _always_ choose a random salt for you using some opaque process. I already have passwords stored in a non-Spring approved format, so I simply can't use Spring for this. 

Instead, I dug a little deeper and discovered that Java EE ships with a bunch of password hashers. It's nice not having a new dependency. Sadly, this is what using it looks like:
```kotlin
val spec = PBEKeySpec(password.toCharArray(), salt, iterations, 32 * 8)
val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
val hashed_password_bytes = factory.generateSecret(spec).getEncoded()
val hashed_password = Base64.getEncoder().encodeToString(hashed_password_bytes)
```

I assume there is some kind of portability concern that requires a string type for the algorithm rather than an enum or something sane, but I certainly can't justify not having a set of required implemented algorithms. I have no idea why they felt it necessary to use `Array<Char>` instead of `String`. 

## JWT key signing
It seems like Spring is not modular enough to offer basic JWT signing and verification without fully committing to the OAuth2 flow, so once again I am forced into a bespoke solution. I found a package called [java-jwt](https://github.com/auth0/java-jwt) that worked just fine. The final result was perfectly reasonable:
```kotlin
val now = Clock.System.now()
val jwt =
  JWT.create()
      .withClaim("sub", "$id")
      .withClaim("iat", now.epochSeconds)
      .withClaim("exp", (now + 1.toDuration(DurationUnit.DAYS)).epochSeconds)
      .withClaim("iss", "go-recordkeeper")
      .withClaim("aud", "go-recordkeeper")
      .sign(Algorithm.HMAC256(config.secretKey))
return "\"$jwt\""
```

The only really questionable bit is how I'm serializing the JWT as a JSON string, but that's on my own head.

Behind the scenes, I was disappointed to find that the Kotlin standard library doesn't have a datetime module. There is an [official package](https://github.com/Kotlin/kotlinx-datetime), but it has to be brought in as its own dependency. Just a small nuisance.

## SQL querying
So far, you may notice that Spring really hasn't been pulling very much weight. That's not going to change here.

There is a lot of Spring infrastructure around abstracting database querying. There's [documentation](https://docs.spring.io/spring-framework/reference/data-access/jdbc/connections.html) about [`DataSource`](https://docs.oracle.com/en/java/javase/17/docs/api/java.sql/javax/sql/DataSource.html) and [`DataSourceBuilder`](https://docs.spring.io/spring-boot/docs/current/api/org/springframework/boot/jdbc/DataSourceBuilder.html) and at least four different ways to configure all the properties required. The idea is to completely isolate the application code from the storage backend so that different databases can be swapped in and out transparently, and DBAs can configure everything without involving developers at all.

This was my solution:
```kotlin
val jdbcUrl = "jdbc:postgresql://${postgresHost}:5432/${postgresName}"
val connection = DriverManager.getConnection(jdbcUrl, postgresUser, postgresPassword)
val query = connection.prepareStatement(statement) // statement is the SQL string
// Set the parameters to the query, they will be sanitized
// ...
val result = query.executeQuery()
```

`DriverManager` and associated functions were all part of the [JDBC](https://en.wikipedia.org/wiki/Java_Database_Connectivity), which I honestly didn't expect to work so smoothly. I admit I'm ignoring connection pooling for now, but I'm honestly surprised that the database connection can be set up in a single function call.

Actually setting the parameters for the query and extracting the values from the result is, frankly, quite painful:
```kotlin
val query = connection.prepareStatement("SELECT id, password FROM auth_user WHERE username=?")
query.setString(1, username) // Set the first parameter
val result = query.executeQuery()
if (!result.next()) {
  // There were no results, the user is clearly unauthorize
  throw ResponseStatusException(HttpStatus.UNAUTHORIZED)
}
// For the current row, get the value from the id and password columns, typed
val id = result.getInt("id")
val db_password = result.getString("password")
// You would loop over result.next() if you wanted to query multiple rows
```

I don't really think there's a better way to do it in Java/Kotlin though, because of the strict and rather underwhelming type system. The accepted solution is to fully buy into an ORM like JPA/Hibernate, which I am absolutely not doing, so the JDBC API will have to do.

Again, Spring is not helpful. This time I'm sure it's possible to do things the Spring way, but it feels much more like an obstruction than a helpful guide rail. Perhaps I will come back to it later.

## Configuration from environment variables
Finally, something Spring shines at. As far as I can tell, most of Spring exists to configure other bits of Spring. You can configure things with properties files, with environment profiles, and with dynamically executed code at run time. Dependencies can be injected as fields or constructor arguments, requested from `BeanFactory`s, or ordered in XML. There are a myriad of classes implementing `ApplicationContext`, which is a `BeanFactory` and a `MessageSource` and a `ResourcePatternResolver`. There are a plethora of `@Annotations` denoting things that are automatically registered through classpath scanning, or registered in a certain order, or depend on other things being registered, or will use some property specified somewhere deliberately abstract and ambiguous.

It's a mess, and I can't claim to understand it. Since I don't understand it, I can't really do anything but whine about it, so I'll just do that. 

The philosophy seems to be that [singletons](https://en.wikipedia.org/wiki/Singleton_pattern) (classes that are instantiated exactly once and recycled everywhere) are the coolest thing imaginable. If you have a conceptual unit in your application, just make a `@Bean` out of it, add it as a dependency wherever it's needed, and Spring will magic it into existence. I can appreciate the upsides of dependency injection. I think [pytest fixtures](https://docs.pytest.org/en/7.1.x/how-to/fixtures.html) and [FastAPI dependencies](https://fastapi.tiangolo.com/tutorial/dependencies/) use the concept to great effect. Using it in Spring, however, is extremely painful.

I spent most of a day trying to figure out how to refer to an environment variable in the login handler. Spring has the concept of a Property, which I think is like a key/value pair in a mapping of Properties that is initially specified in an `application.properties` file. The only documentation I could find suggested that this `application.properties` file could reference environment variables to define properties, which seemed true as far as I could tell. So, I set up some Properties to intermediate between the environment variables, then tried to figure out how to inject them into the login handler.

Some googling suggested that the `@Value` annotation needs to be applied to a field on the Controller class, which Spring will then populate with the property. This worked, except for when the environment variable was not defined. In that case an unrecoverable runtime error was thrown. Sadly, my specification includes a `GOBAN_SECRET_KEY` environment variable which reverts to a default if not defined, so this will not work for me.

Since I couldn't find a functioning way to refer to environment variables in Spring, I ripped it all out and used Java's native [`System.getenv`](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#getenv--) to get the environment variables instead. This is cool, but some of the values need processing before they are usable. The `GOBAN_DEVELOPMENT` var should correspond to a boolean `true` if it maps the string `true`, or `false` otherwise (including if it is unset). `GOBAN_SECRET_KEY` needs to have a default value if unset. It would be cool to have runtime Properties like `gobanDevelopment` and `gobanSecretKey` that already have this logic applied so that they can individually be injected when required.

That's not possible, as far as I can tell. It seems like it should be possible, but I couldn't get it working. You should be able to register a lifecycle listener with an annotation that will inject the properties after the application is sufficiently initialized, but I couldn't get a listener to fire. You should be able to configure the application before you start it in your `main` method, but Kotlin uses this bizarre `runApplication` method that I cannot find documented anywhere, so I couldn't figure out how to feed it a modified application.

Instead, I opted to create a `GobanConfig` singleton and just inject the whole thing. After bashing my head against the problem for a few more hours, I discovered that Spring does not do bean injection into method arguments. It's at least vaguely aware that you might want to do such a thing, because request parameters are injected just fine, but if you try to inject a `Bean`, it will just instantiate the class with no arguments rather than attempt any bean resolution. You may, however, inject beans into *constructor* arguments with no problems. That is, if you remembered to name the method returning the `Bean` the same thing as the constructor argument. Apparently `@Autowired`, `@Inject`, and `@Resource` will all do similar but fundamentally different things when trying to resolve a bean, so take that with a grain of salt.

After all that, I'm not really any closer to understanding *why* Spring does the things it does. There are a myriad of ways of achieving anything that all vaguely overlap, which makes it very difficult for me to know if I'm on the right trail to get somewhere. Even with logging cranked all the way up, I had no visibility into what the classpath scanning magic was doing, or if any of the annotations I was experimenting with were having any effect. Spring is clearly doing a lot of stuff, but I could not discover the correct incantations to get it to do the stuff I want. Perhaps some of it is my IDE's fault. I'm using a pretty bare bones Kotlin LSP in neovim, so I don't have a lot of the Java tooling you would find in Eclipse or IntelliJ. I remember exploring a lot of Java source code in the past, which neovim wouldn't let me do.

There is also a vast miasma of SEO articles on every conceivable set of Spring related keywords that will provide a small slice of information on how to perform an extremely specific task, but provide no context into the actual functionality or design of the underlying systems. [Baeldung](https://www.baeldung.com) is the worst offender, although I was also disappointed with the state of the Spring docs themselves. The general purpose documentations was scant on details and seems to have had several editions over the years, which the Javadocs just describe exactly what every function does without any context as to why it would do that or what consequences that might have. I don't know how you would build a mental model of how Spring works without spending years working around it and dissecting it. All the magic that Spring promises will improve the lives of developers is just opaque, incomprehensible witchcraft that cannot be invoked without knowledge that I don't have the knowledge to find.

# Next steps
Do the register endpoint, now that the login endpoint is working. I'm hardly using anything Spring related, so as long as I don't touch anything I shouldn't need to suffer any more than I already have. Fingers crossed.
