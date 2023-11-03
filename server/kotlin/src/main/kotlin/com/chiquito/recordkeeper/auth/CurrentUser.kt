package com.chiquito.recordkeeper.auth

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import jakarta.servlet.Filter
import jakarta.servlet.http.HttpServletRequest
import java.sql.ResultSet
import java.util.regex.*
import org.springframework.context.annotation.Bean
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

private val AUTHORIZATION_HEADER_REGEX = Pattern.compile("Bearer (.*)")

@RestController
class CurrentUserController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Response(val id: Int, val username: String, val email: String)

  private fun parseAuthorizationHeader(header: String?): Int {
    if (header != null) {
      val matcher = AUTHORIZATION_HEADER_REGEX.matcher(header)
      if (matcher.matches()) {
        val jwt = matcher.group(1)!!
        val decoded =
            JWT.require(Algorithm.HMAC256(config.secretKey)).acceptLeeway(10).build().verify(jwt)
        if (decoded != null) {
          return decoded.getSubject()!!.toInt()
        }
      }
    }
    throw ResponseStatusException(HttpStatus.FORBIDDEN)
  }

  private fun lookupUser(id: Int): ResultSet {
    val query = config.statement("SELECT username, email, is_active FROM auth_user WHERE id=?")
    query.setInt(1, id)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.FORBIDDEN, "not allowed")
    }
    return result
  }

  @GetMapping("/api/user/")
  @ResponseBody
  fun getUser(
      @RequestHeader(HttpHeaders.AUTHORIZATION) authorization: String?
  ): ResponseEntity<Response> {
    val id = parseAuthorizationHeader(authorization)
    val result = lookupUser(id)
    val username = result.getString("username")
    val email = result.getString("email")
    return ResponseEntity.ok().body(Response(id, username, email))
  }

  /**
   * Set the getUser request attribute so that any endpoints that require authentication can use it
   * to parse the Authorization header to get the user ID and verify that it exists in the DB.
   */
  @Bean
  public fun authenticationFilter(): Filter {
    return Filter({ request, response, chain ->
      val request = request as HttpServletRequest
      val authorization = request.getHeader("Authorization")
      request.setAttribute(
          "getUser",
          {
            // This will throw an exception if the JWT does not contain a valid ID
            val id = parseAuthorizationHeader(authorization)
            // This will throw an exception if the user does not exist in the DB
            lookupUser(id)
            id
          }
      )
      chain.doFilter(request, response)
    })
  }
}
