package com.chiquito.recordkeeper.auth

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.util.regex.*
import kotlinx.datetime.Clock
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

private val AUTHORIZATION_HEADER_REGEX = Pattern.compile("Bearer (.*)")

@RestController
class CurrentUserController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Response(val id: Int, val username: String, val email: String)

  fun parseAuthorizationHeader(header: String?): Int {
    if (header != null) {
      val matcher = AUTHORIZATION_HEADER_REGEX.matcher(header)
      if (matcher.matches()) {
        val jwt = matcher.group(1)!!
        val decoded = JWT.require(Algorithm.HMAC256(config.secretKey)).build().verify(jwt)
        if (decoded != null) {
          return decoded.getSubject()!!.toInt()
        }
      }
    }
    throw ResponseStatusException(HttpStatus.FORBIDDEN)
  }

  @GetMapping("/api/user/")
  @ResponseBody
  fun getUser(
      @RequestHeader(HttpHeaders.AUTHORIZATION) authorization: String?
  ): ResponseEntity<Response> {
    val id = parseAuthorizationHeader(authorization)
    val query = config.statement("SELECT username, email, is_active FROM auth_user WHERE id=?")
    val now = java.sql.Timestamp(Clock.System.now().toEpochMilliseconds())
    query.setInt(1, id)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.FORBIDDEN, "not allowed")
    }
    val username = result.getString("username")
    val email = result.getString("email")
    return ResponseEntity.ok().body(Response(id, username, email))
  }
}
