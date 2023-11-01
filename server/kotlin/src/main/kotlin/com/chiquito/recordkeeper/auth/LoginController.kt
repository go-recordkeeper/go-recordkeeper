package com.chiquito.recordkeeper.auth

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.util.Base64
import java.util.regex.*
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import kotlin.time.DurationUnit
import kotlin.time.toDuration
import kotlinx.datetime.Clock
import org.springframework.http.HttpStatus
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.ResponseBody
import org.springframework.web.bind.annotation.RestController
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class LoginController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Request(val username: String, val password: String)

  @PostMapping("/api/login/")
  @ResponseBody
  fun login(@RequestBody request: Request): String {
    val (username, password) = request
    val query = config.statement("SELECT id, password FROM auth_user WHERE username=?")
    query.setString(1, username)
    val result = query.executeQuery()
    if (!result.next()) {
      // TODO do a hash here to avoid timing attacks
      throw ResponseStatusException(HttpStatus.UNAUTHORIZED)
    }
    val id = result.getInt("id")
    val db_password = result.getString("password")
    val pattern =
        Pattern.compile("^pbkdf2_sha256\\\$([0-9]+)\\\$([a-zA-Z0-9+/]+)\\\$([a-zA-Z0-9+/=]+)\$")
    val matcher = pattern.matcher(db_password)
    if (!matcher.matches()) {
      throw ResponseStatusException(HttpStatus.UNAUTHORIZED)
    }
    val iterations = matcher.group(1).toInt()
    val salt = matcher.group(2).toByteArray()
    val actual_hashed_password = matcher.group(3)

    val spec = PBEKeySpec(password.toCharArray(), salt, iterations, 32 * 8)
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val hashed_password_bytes = factory.generateSecret(spec).getEncoded()
    val hashed_password = Base64.getEncoder().encodeToString(hashed_password_bytes)

    if (hashed_password.equals(actual_hashed_password)) {
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
    } else {
      throw ResponseStatusException(HttpStatus.UNAUTHORIZED)
    }
  }
}
