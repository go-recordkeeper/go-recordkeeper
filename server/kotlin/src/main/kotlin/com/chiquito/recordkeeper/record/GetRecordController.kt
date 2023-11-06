package com.chiquito.recordkeeper.record

import com.chiquito.recordkeeper.*
import io.github.oshai.kotlinlogging.KotlinLogging
import java.sql.Timestamp
import java.util.Optional
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class GetRecordController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Point(val x: Int, val y: Int)
  data class ResponseMove(
      val position: Optional<Point>,
      val color: String,
      val captures: List<Point>
  )
  data class ResponseStone(val x: Int, val y: Int, val color: String)
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
      val moves: List<ResponseMove>,
      val stones: List<ResponseStone>,
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

    val movesQuery =
        config.statement(
            "SELECT position, color FROM record_move WHERE record_id=? ORDER BY move ASC"
        )
    movesQuery.setInt(1, recordId)

    val movesResult = movesQuery.executeQuery()
    val board = GoBoard(boardSize)
    val moves: ArrayList<ResponseMove> = ArrayList(20)
    while (movesResult.next()) {
      val color = Color.valueOf(movesResult.getString("color"))
      val pos = board.fromIndex(movesResult.getInt("position"))
      if (movesResult.wasNull()) {
        // moves.add(GoBoard.Move(color, Optional.empty()))
        moves.add(ResponseMove(Optional.empty(), color.toString(), listOf()))
      } else {
        val captures = board.placeStone(color, pos).map { Point(it.x, it.y) }
        // moves.add(GoBoard.Move(color, Optional.of(board.fromIndex(position))))
        moves.add(ResponseMove(Optional.of(Point(pos.x, pos.y)), color.toString(), captures))
      }
    }
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
                board.stones.asIterable().sortedBy { it.key.index }.map {
                  ResponseStone(it.key.x, it.key.y, it.value.toString())
                },
            )
        )
  }
}
