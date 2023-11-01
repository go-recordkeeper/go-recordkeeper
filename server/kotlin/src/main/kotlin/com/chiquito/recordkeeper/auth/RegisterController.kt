package com.chiquito.recordkeeper.auth

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.security.SecureRandom
import java.util.Base64
import java.util.regex.*
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import kotlinx.datetime.Clock
import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestBody
import org.springframework.web.bind.annotation.ResponseBody
import org.springframework.web.bind.annotation.RestController
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class RegisterController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Request(val username: String, val email: String, val password: String)
  data class Response(val id: Int, val username: String, val email: String)

  fun isValidEmail(email: String): Boolean {
    return EMAIL_REGEX.matcher(email).matches()
  }

  fun generateSalt(): ByteArray {
    val whitelist = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val random = SecureRandom()
    val salt = ByteArray(16)
    for (i in 0 ..< 16) {
      val c = whitelist[random.nextInt(whitelist.length)]
      salt[i] = c.code.toByte()
    }
    return salt
  }

  fun hashPassword(password: String): String {
    val salt = generateSalt()
    val iterations = 39000
    val spec = PBEKeySpec(password.toCharArray(), salt, iterations, 32 * 8)
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val hashed_password_bytes = factory.generateSecret(spec).getEncoded()
    val hashed_password = Base64.getEncoder().encodeToString(hashed_password_bytes)
    val salt_string = salt.toString(Charsets.UTF_8)
    return "pbkdf2_sha256\$${iterations}\$${salt_string}\$${hashed_password}"
  }

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
    val passwordHashed = hashPassword(password)
    query.setString(3, passwordHashed)
    query.setTimestamp(4, now)
    query.setTimestamp(5, now)
    try {
      val result = query.executeQuery()
      if (!result.next()) {
        throw ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR)
      }
      val id = result.getInt("id")
      return ResponseEntity.status(201).body(Response(id, username, email))
    } catch (e: org.postgresql.util.PSQLException) {
      if (e.getServerErrorMessage()?.getSQLState() == "23505") {
        throw ResponseStatusException(
            HttpStatus.BAD_REQUEST,
            "A user with that username already exists."
        )
      }
      throw ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }
}

// I put these regexes at the bottom because my editor is bad at recognizing string escape sequences
// and barfs all over my syntax highlighting
private val EMAIL_REGEX = Pattern.compile(".*@.*\\..*")
