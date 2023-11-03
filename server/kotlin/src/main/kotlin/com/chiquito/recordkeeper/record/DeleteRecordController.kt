package com.chiquito.recordkeeper.auth

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.sql.Timestamp
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class DeleteRecordController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Record(
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
  data class Response(
      val count: Int,
      val pages: Int,
      val results: List<Record>,
  )

  @DeleteMapping("/api/records/{recordId}/")
  fun deleteRecord(
      @PathVariable recordId: Int,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Unit> {
    val ownerId = getUser()
    // Verify the record exists before we try to delete it
    val query = config.statement("SELECT id FROM record_record WHERE owner_id=? AND id=?")
    query.setInt(1, ownerId)
    query.setInt(2, recordId)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND)
    }
    // Delete the moves first because Django didn't set up cascading deletes properly
    val deleteMovesQuery = config.statement("DELETE FROM record_move WHERE record_id=?")
    deleteMovesQuery.setInt(1, recordId)
    deleteMovesQuery.execute()
    // Delete the moves first because Django didn't set up cascading deletes properly
    val deleteQuery = config.statement("DELETE FROM record_record WHERE owner_id=? AND id=?")
    deleteQuery.setInt(1, ownerId)
    deleteQuery.setInt(2, recordId)
    deleteQuery.execute()
    return ResponseEntity.noContent().build()
  }
}
