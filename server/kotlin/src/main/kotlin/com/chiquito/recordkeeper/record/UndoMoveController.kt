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
class UndoMoveController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  data class Stone(val color: Color, val x: Int, val y: Int)
  data class Point(val x: Int, val y: Int)
  data class Response(val add: List<Stone>, val remove: List<Point>)

  @PostMapping("/api/records/{recordId}/undo/")
  @ResponseBody
  fun playMove(
      @PathVariable recordId: Int,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<Response> {
    val ownerId = getUser()

    val query = config.statement("SELECT board_size FROM record_record WHERE owner_id=? AND id=?")
    query.setInt(1, ownerId)
    query.setInt(2, recordId)
    val result = query.executeQuery()
    if (!result.next()) {
      throw ResponseStatusException(HttpStatus.NOT_FOUND)
    }

    val boardSize = result.getInt("board_size")

    val board = GoBoard(boardSize)

    val movesQuery =
        config.statement(
            "SELECT position, color FROM record_move WHERE record_id=? ORDER BY move ASC"
        )
    movesQuery.setInt(1, recordId)

    val movesResult = movesQuery.executeQuery()
    val moves = ArrayList<GoBoard.Move>()
    // dumb initialization values that should never be used
    var lastColor: Color = Color.B
    var lastPos: GoBoard.Position = board.fromIndex(0)
    var lastCaptures: List<GoBoard.Position> = emptyList()
    var lastWasPass: Boolean = false
    while (movesResult.next()) {
      lastColor = Color.valueOf(movesResult.getString("color"))
      lastPos = board.fromIndex(movesResult.getInt("position"))
      if (movesResult.wasNull()) {
        moves.add(GoBoard.Move(lastColor, Optional.empty()))
        lastWasPass = true
      } else {
        lastCaptures = board.placeStone(lastColor, lastPos)
        moves.add(GoBoard.Move(lastColor, Optional.of(lastPos)))
        lastWasPass = false
      }
    }
    if (moves.isEmpty()) {
      throw ResponseStatusException(HttpStatus.FORBIDDEN, "No moves to undo")
    }

    val undoQuery =
        config.statement(
            "DELETE FROM record_move WHERE record_id=? AND move=?",
        )
    undoQuery.setInt(1, recordId)
    undoQuery.setInt(2, moves.size)
    undoQuery.execute()

    return ResponseEntity.ok()
        .body(
            Response(
                lastCaptures.map { Stone(lastColor.invert(), it.x, it.y) },
                if (lastWasPass) emptyList() else listOf(Point(lastPos.x, lastPos.y))
            )
        )
  }
}
