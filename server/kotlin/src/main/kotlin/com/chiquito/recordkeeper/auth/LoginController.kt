package com.chiquito.recordkeeper.auth

import io.github.oshai.kotlinlogging.KotlinLogging
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.RestController

private val logger = KotlinLogging.logger {}

@RestController
class LoginController {

  data class Request(val username: String, val password: String)

  @PostMapping("/api/login/")
  fun login(@RequestBody x: Request): String {
    logger.debug { "logging in as $x" }
    return "\"jwt token for real trust me\""
  }
}
