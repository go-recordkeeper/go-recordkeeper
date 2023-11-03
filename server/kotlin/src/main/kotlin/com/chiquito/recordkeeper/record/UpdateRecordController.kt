package com.chiquito.recordkeeper.auth

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.sql.Timestamp
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*

private val logger = KotlinLogging.logger {}

@RestController
class UpdateRecordsController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Request(
      val name: String,
      val black_player: String,
      val white_player: String,
      val comment: String,
      val handicap: Int,
      val komi: Double,
      val ruleset: String,
      val winner: String,
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

  @PutMapping("/api/records/{recordId}/")
  @ResponseBody
  fun createRecord(
      @PathVariable recordId: Int,
      @RequestBody request: Request,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Response> {
    val ownerId = getUser()

    val query =
        config.statement(
            "UPDATE record_record SET name=?, black_player=?, white_player=?, comment=?, handicap=?, komi=?, ruleset=?, winner=? WHERE owner_id=? AND id=? RETURNING board_size, created"
        )
    query.setString(1, request.name)
    query.setString(2, request.black_player)
    query.setString(3, request.white_player)
    query.setString(4, request.comment)
    query.setInt(5, request.handicap)
    query.setDouble(6, request.komi)
    query.setString(7, request.ruleset)
    query.setString(8, request.winner)
    query.setInt(9, ownerId)
    query.setInt(10, recordId)
    val result = query.executeQuery()

    // TODO error handling
    result.next()
    return ResponseEntity.ok()
        .body(
            Response(
                recordId,
                ownerId,
                result.getInt("board_size"),
                result.getTimestamp("created"),
                request.name,
                request.black_player,
                request.white_player,
                request.comment,
                request.handicap,
                request.komi,
                request.ruleset,
                request.winner
            )
        )
  }
}
