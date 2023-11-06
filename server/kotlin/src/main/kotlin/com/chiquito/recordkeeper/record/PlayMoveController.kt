package com.chiquito.recordkeeper.record

import com.chiquito.recordkeeper.*
import io.github.oshai.kotlinlogging.KotlinLogging
import java.util.Optional
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class PlayMoveController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Stone(val color: Color, val x: Int, val y: Int)
  data class Point(val x: Int, val y: Int)
  data class Response(val add: List<Stone>, val remove: List<Point>)

  @PostMapping("/api/records/{recordId}/play/")
  @ResponseBody
  fun playMove(
      @RequestBody request: Point,
      @PathVariable recordId: Int,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Response> {
    val ownerId = getUser()

    val query =
        config.statement("SELECT board_size, handicap FROM record_record WHERE owner_id=? AND id=?")
    query.setInt(1, ownerId)
    query.setInt(2, recordId)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND)
    }

    val boardSize = result.getInt("board_size")
    val handicap = result.getInt("handicap")

    val board = GoBoard(boardSize)
    val pos = board.fromCoordinates(request.x, request.y)

    val movesQuery =
        config.statement(
            "SELECT position, color FROM record_move WHERE record_id=? ORDER BY move ASC"
        )
    movesQuery.setInt(1, recordId)

    val movesResult = movesQuery.executeQuery()
    val moves = ArrayList<GoBoard.Move>()
    while (movesResult.next()) {
      val color = Color.valueOf(movesResult.getString("color"))
      val position = movesResult.getInt("position")
      if (movesResult.wasNull()) {
        moves.add(GoBoard.Move(color, Optional.empty()))
      } else {
        board.placeStone(color, board.fromIndex(position))
        moves.add(GoBoard.Move(color, Optional.of(board.fromIndex(position))))
      }
    }

    val color =
        if (moves.isEmpty() || moves.size < handicap) {
          Color.B
        } else {
          moves.last().color.invert()
        }

    val captures: List<Point>
    try {
      captures = board.placeStone(color, pos).map { Point(it.x, it.y) }
    } catch (e: OutOfBoundsException) {
      throw ResponseStatusException(HttpStatus.FORBIDDEN, "Out of bounds")
    } catch (e: SpaceOccupiedException) {
      throw ResponseStatusException(HttpStatus.FORBIDDEN, "Already a stone there")
    } catch (e: SuicideException) {
      throw ResponseStatusException(HttpStatus.FORBIDDEN, "Move is suicidal")
    }

    val playQuery =
        config.statement(
            "INSERT INTO record_move (record_id, position, color, move) VALUES (?, ?, ?, ?)",
        )
    playQuery.setInt(1, recordId)
    playQuery.setInt(2, pos.index)
    playQuery.setString(3, color.toString())
    playQuery.setInt(4, moves.size + 1)
    playQuery.execute()

    return ResponseEntity.status(201)
        .body(Response(listOf(Stone(color, request.x, request.y)), captures))
  }
}
