package com.chiquito.recordkeeper

import io.github.oshai.kotlinlogging.KotlinLogging
import jakarta.servlet.Filter
import java.sql.Connection
import java.sql.DriverManager
import java.sql.PreparedStatement
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean

private val logger = KotlinLogging.logger {}

data class GobanConfig(
    val development: Boolean,
    val secretKey: String,
    val postgresName: String,
    val postgresUser: String,
    val postgresPassword: String,
    val postgresHost: String,
    val connection: Connection,
) {
  fun statement(statement: String): PreparedStatement {
    return connection.prepareStatement(statement)
  }

  @Bean
  public fun connectionCleanup(): Filter {
    return Filter({ request, response, chain ->
      chain.doFilter(request, response)
      connection.close()
    })
  }
}

@SpringBootApplication
class KotlinApplication {

  @Bean
  public fun gobanConfig(): GobanConfig {
    val development = "true".equals(System.getenv("GOBAN_DEVELOPMENT"))
    val envSecretKey = System.getenv("GOBAN_SECRET_KEY")
    val secretKey =
        if (envSecretKey != null) envSecretKey
        else "django-insecure-(@ppnpk\$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj"
    val postgresName = System.getenv("POSTGRES_NAME") ?: ""
    val postgresUser = System.getenv("POSTGRES_USER") ?: ""
    val postgresPassword = System.getenv("POSTGRES_PASSWORD") ?: ""
    val postgresHost = System.getenv("POSTGRES_HOST") ?: ""
    val jdbcUrl = "jdbc:postgresql://${postgresHost}:5432/${postgresName}"
    val connection = DriverManager.getConnection(jdbcUrl, postgresUser, postgresPassword)
    return GobanConfig(
        development,
        secretKey,
        postgresName,
        postgresUser,
        postgresPassword,
        postgresHost,
        connection
    )
  }
}

fun main(args: Array<String>) {
  runApplication<KotlinApplication>(*args) {}
}
