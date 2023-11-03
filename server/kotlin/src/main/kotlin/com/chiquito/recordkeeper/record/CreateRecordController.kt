package com.chiquito.recordkeeper.auth

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.sql.Timestamp
import java.util.Optional
import java.util.regex.*
import kotlinx.datetime.Clock
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class CreateRecordsController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Request(
      val board_size: Int,
      val name: Optional<String>,
      val black_player: Optional<String>,
      val white_player: Optional<String>,
      val comment: Optional<String>,
      val handicap: Optional<Int>,
      val komi: Optional<Double>,
      val ruleset: Optional<String>,
  )
  data class Response(
      val id: Int,
      val owner: Int,
      val board_size: Int,
      val created: Timestamp,
      val name: String,
      val black_player: String,
      val white_player: String,
      val comment: String,
      val handicap: Int,
      val komi: Double,
      val ruleset: String,
      val winner: String,
  )

  @PostMapping("/api/records/")
  @ResponseBody
  fun createRecord(
      @RequestBody request: Request,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Response> {
    val ownerId = getUser()

    if (!(request.board_size == 9 || request.board_size == 13 || request.board_size == 19)) {
      throw ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid board size.")
    }

    val name = request.name.orElse("")
    val blackPlayer = request.black_player.orElse("Black")
    val whitePlayer = request.white_player.orElse("White")
    val comment = request.comment.orElse("")
    val handicap = request.handicap.orElse(0)
    val komi = request.komi.orElse(7.5)
    val ruleset = request.ruleset.orElse("AGA")
    val winner = "U"
    val now = Timestamp(Clock.System.now().toEpochMilliseconds())

    val query =
        config.statement(
            "INSERT INTO record_record (owner_id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
        )
    query.setInt(1, ownerId)
    query.setInt(2, request.board_size)
    query.setString(3, name)
    query.setString(4, blackPlayer)
    query.setString(5, whitePlayer)
    query.setString(6, comment)
    query.setInt(7, handicap)
    query.setDouble(8, komi)
    query.setString(9, ruleset)
    query.setString(10, winner)
    query.setTimestamp(11, now)
    val result = query.executeQuery()

    // TODO error handling
    result.next()
    return ResponseEntity.status(201)
        .body(
            Response(
                result.getInt("id"),
                ownerId,
                request.board_size,
                now,
                name,
                blackPlayer,
                whitePlayer,
                comment,
                handicap,
                komi,
                ruleset,
                winner
            )
        )
  }
}
