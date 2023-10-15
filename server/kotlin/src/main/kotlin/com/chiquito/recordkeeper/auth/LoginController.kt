package com.chiquito.recordkeeper.auth

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import io.github.oshai.kotlinlogging.KotlinLogging
import kotlin.time.DurationUnit
import kotlin.time.toDuration
import kotlinx.datetime.Clock
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.ResponseBody
import org.springframework.web.bind.annotation.RestController

private val logger = KotlinLogging.logger {}

@RestController
class LoginController {

  data class Request(val username: String, val password: String)

  @PostMapping("/api/login/")
  @ResponseBody
  fun login(@RequestBody request: Request): String {
    val (username, password) = request
    // TODO hook up DB
    // TODO pbkdf2 to check password
    val now = Clock.System.now()
    val jwt =
        JWT.create()
            .withClaim("sub", "666")
            .withClaim("iat", now.epochSeconds)
            .withClaim("exp", (now + 1.toDuration(DurationUnit.DAYS)).epochSeconds)
            .withClaim("iss", "go-recordkeeper")
            .withClaim("aud", "go-recordkeeper")
            // TODO grab secret key from env config
            .sign(
                Algorithm.HMAC256(
                    "django-insecure-(@ppnpk\$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj"
                )
            )
    return "\"$jwt\""
  }
}
