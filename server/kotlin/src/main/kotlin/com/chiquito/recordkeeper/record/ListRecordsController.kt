package com.chiquito.recordkeeper.record

import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.sql.Timestamp
import java.util.Optional
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class ListRecordsController(gobanConfig: GobanConfig) {
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

  @GetMapping("/api/records/")
  @ResponseBody
  fun listRecords(
      @RequestAttribute("getUser") getUser: () -> Int,
      @RequestParam("page") page: Optional<Int>,
      @RequestParam("page_size") pageSize: Optional<Int>,
  ): ResponseEntity<Response> {
    val ownerId = getUser()
    val page = page.orElse(1)
    val pageSize = pageSize.orElse(10)
    if (pageSize < 1) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND, "Invalid page size.")
    }
    val countQuery = config.statement("SELECT COUNT(*) FROM record_record WHERE owner_id = ?")
    countQuery.setInt(1, ownerId)
    val countResult = countQuery.executeQuery()
    if (!countResult.next()) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND)
    }
    val count = countResult.getInt(1)
    val pages = (maxOf(count, 1) + pageSize - 1) / pageSize
    if (page < 1 || page > pages) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND, "Invalid page number.")
    }
    val query =
        config.statement(
            "SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = ? ORDER BY CREATED DESC LIMIT ? OFFSET ?"
        )
    query.setInt(1, ownerId)
    query.setInt(2, pageSize)
    query.setInt(3, (page - 1) * pageSize)
    val result = query.executeQuery()

    val records = ArrayList<Record>(pageSize)
    while (result.next()) {
      records.add(
          Record(
              result.getInt("id"),
              ownerId,
              result.getInt("board_size"),
              result.getTimestamp("created"),
              result.getString("name"),
              result.getString("black_player"),
              result.getString("white_player"),
              result.getString("comment"),
              result.getInt("handicap"),
              result.getDouble("komi"),
              result.getString("ruleset"),
              result.getString("winner"),
          )
      )
    }
    return ResponseEntity.ok().body(Response(count, pages, records))
  }
}
