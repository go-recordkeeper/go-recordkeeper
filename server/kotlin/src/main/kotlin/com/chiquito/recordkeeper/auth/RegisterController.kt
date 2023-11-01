package com.chiquito.recordkeeper.auth

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.util.regex.*
import kotlinx.datetime.Clock
import org.postgresql.util.PSQLException
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.ResponseBody
import org.springframework.web.bind.annotation.RestController
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

private val EMAIL_REGEX = Pattern.compile(".*@.*\\..*")

fun isValidEmail(email: String): Boolean {
  return EMAIL_REGEX.matcher(email).matches()
}

@RestController
class RegisterController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Request(val username: String, val email: String, val password: String)
  data class Response(val id: Int, val username: String, val email: String)

  @PostMapping("/api/register/")
  @ResponseBody
  fun register(@RequestBody request: Request): ResponseEntity<Response> {
    val (username, email, password) = request
    if (!isValidEmail(email)) {
      throw ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid email.")
    }

    val query =
        config.statement(
            "INSERT INTO auth_user (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active) VALUES (?, ?, ?, ?, ?, '', '', false, false, true) RETURNING id"
        )
    val now = java.sql.Timestamp(Clock.System.now().toEpochMilliseconds())
    query.setString(1, username)
    query.setString(2, email)
    // TODO hash the password
    val passwordHashed = "abc123"
    query.setString(3, passwordHashed)
    query.setTimestamp(4, now)
    query.setTimestamp(5, now)
    try {
      val result = query.executeQuery()
      if (!result.next()) {
        // TODO what do
        throw ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR)
      }
      val id = result.getInt("id")
      return ResponseEntity.status(201).body(Response(id, username, email))
    } catch (e: org.postgresql.util.PSQLException) {
      logger.debug { e.getServerErrorMessage()!!.getSQLState() }
      if (e.getServerErrorMessage()?.getSQLState() == "23505") {
        throw ResponseStatusException(
            HttpStatus.BAD_REQUEST,
            "A user with that username already exists."
        )
      }
      throw ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR)
    }
    // val pattern =
    //     Pattern.compile("^pbkdf2_sha256\\\$([0-9]+)\\\$([a-zA-Z0-9+/]+)\\\$([a-zA-Z0-9+/=]+)\$")
    // val matcher = pattern.matcher(db_password)
    // if (!matcher.matches()) {
    //   throw ResponseStatusException(HttpStatus.UNAUTHORIZED)
    // }
    // val iterations = matcher.group(1).toInt()
    // val salt = matcher.group(2).toByteArray()
    // val actual_hashed_password = matcher.group(3)
    //
    // // val random = SecureRandom()
    // // val salt = ByteArray(16)
    // // random.nextBytes(salt)
    // val spec = PBEKeySpec(password.toCharArray(), salt, iterations, 32 * 8)
    // val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    // val hashed_password_bytes = factory.generateSecret(spec).getEncoded()
    // val hashed_password = Base64.getEncoder().encodeToString(hashed_password_bytes)
    //
    // if (hashed_password.equals(actual_hashed_password)) {
    //   val now = Clock.System.now()
    //   val jwt =
    //       JWT.create()
    //           .withClaim("sub", "666")
    //           .withClaim("iat", now.epochSeconds)
    //           .withClaim("exp", (now + 1.toDuration(DurationUnit.DAYS)).epochSeconds)
    //           .withClaim("iss", "go-recordkeeper")
    //           .withClaim("aud", "go-recordkeeper")
    //           // TODO grab secret key from env config
    //           .sign(Algorithm.HMAC256(config.secretKey))
    //   return "\"$jwt\""
    // } else {
    //   throw ResponseStatusException(HttpStatus.UNAUTHORIZED)
    // }
  }
}
