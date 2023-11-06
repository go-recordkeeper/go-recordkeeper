package com.chiquito.recordkeeper.record

import com.chiquito.recordkeeper.*
import io.github.oshai.kotlinlogging.KotlinLogging
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class PassTurnController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  @PostMapping("/api/records/{recordId}/pass/")
  fun passTurn(
      @PathVariable recordId: Int,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Unit> {
    val ownerId = getUser()

    val query = config.statement("SELECT handicap FROM record_record WHERE owner_id=? AND id=?")
    query.setInt(1, ownerId)
    query.setInt(2, recordId)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND)
    }

    val handicap = result.getInt("handicap")

    val movesQuery =
        config.statement(
            "SELECT move, color FROM record_move WHERE record_id=? ORDER BY move DESC LIMIT 1"
        )
    movesQuery.setInt(1, recordId)
    val movesResult = movesQuery.executeQuery()

    val passQuery =
        config.statement(
            "INSERT INTO record_move (record_id, position, color, move) VALUES (?, ?, ?, ?)",
        )
    passQuery.setInt(1, recordId)
    passQuery.setNull(2, java.sql.Types.INTEGER)
    if (!movesResult.next()) {
      passQuery.setString(3, "B")
      passQuery.setInt(4, 1)
    } else {
      val lastMove = movesResult.getInt("move")
      val lastColor = Color.valueOf(movesResult.getString("color"))
      if (lastMove < handicap) {
        passQuery.setString(3, "B")
      } else {
        passQuery.setString(3, lastColor.invert().toString())
      }
      passQuery.setInt(4, lastMove + 1)
    }
    passQuery.execute()

    return ResponseEntity.status(201).build()
  }
}
