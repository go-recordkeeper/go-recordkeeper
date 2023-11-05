package com.chiquito.recordkeeper.record

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.sql.Timestamp
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class GetRecordController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

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
      val moves: Array<Int>,
      val stones: Array<Int>,
  )

  @GetMapping("/api/records/{recordId}/")
  @ResponseBody
  fun getRecord(
      @PathVariable recordId: Int,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Response> {
    val ownerId = getUser()
    val query =
        config.statement(
            "SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = ? AND id = ?"
        )
    query.setInt(1, ownerId)
    query.setInt(2, recordId)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND)
    }
    val boardSize = result.getInt("board_size")
    val created = result.getTimestamp("created")
    val name = result.getString("name")
    val blackPlayer = result.getString("black_player")
    val whitePlayer = result.getString("white_player")
    val comment = result.getString("comment")
    val handicap = result.getInt("handicap")
    val komi = result.getDouble("komi")
    val ruleset = result.getString("ruleset")
    val winner = result.getString("winner")
    // TODO calculate moves and stones
    val moves: Array<Int> = emptyArray()
    val stones: Array<Int> = emptyArray()
    return ResponseEntity.ok()
        .body(
            Response(
                recordId,
                ownerId,
                boardSize,
                created,
                name,
                blackPlayer,
                whitePlayer,
                comment,
                handicap,
                komi,
                ruleset,
                winner,
                moves,
                stones,
            )
        )
  }
}
