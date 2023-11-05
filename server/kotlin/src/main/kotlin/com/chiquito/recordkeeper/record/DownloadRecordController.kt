package com.chiquito.recordkeeper.auth

import com.chiquito.recordkeeper.GoBoard
import com.chiquito.recordkeeper.GobanConfig
import io.github.oshai.kotlinlogging.KotlinLogging
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.Locale
import java.util.regex.*
import org.springframework.http.*
import org.springframework.web.bind.annotation.*
import org.springframework.web.server.ResponseStatusException

private val logger = KotlinLogging.logger {}

@RestController
class DownloadRecordController(gobanConfig: GobanConfig) {
  private val config: GobanConfig = gobanConfig

  @GetMapping("/api/records/{recordId}/download/")
  @ResponseBody
  fun downloadRecord(
      @PathVariable recordId: Int,
      @RequestAttribute("getUser") getUser: () -> Int,
  ): ResponseEntity<String> {
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
    val blackPlayer = result.getString("black_player")
    val whitePlayer = result.getString("white_player")
    val handicap = result.getInt("handicap")
    val komi = result.getDouble("komi")
    val ruleset = result.getString("ruleset")
    val created = result.getTimestamp("created")

    val _name = result.getString("name")
    val name = if (_name != "") "GN[${_name}]" else ""

    val _comment = result.getString("comment")
    val comment = if (_comment != "") "GC[${_comment}]" else ""

    val _winner = result.getString("winner")
    val winner =
        if (_winner == "U") "Void"
        else if (_winner == "B") "B+R"
        else if (_winner == "W") "W+R"
        else throw ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR)

    val movesQuery =
        config.statement(
            "SELECT position, color FROM record_move WHERE record_id=? ORDER BY move ASC"
        )
    movesQuery.setInt(1, recordId)
    val movesResult = movesQuery.executeQuery()
    val board = GoBoard(boardSize)
    val moves = StringBuilder()
    while (movesResult.next()) {
      val dbPosition = movesResult.getInt("position")
      val positionCode =
          if (dbPosition == null) "tt"
          else {
            val position = board.fromIndex(dbPosition)
            val xChar = (position.x + 'a'.code.toInt()).toChar()
            val yChar = (position.y + 'a'.code.toInt()).toChar()
            "${xChar}${yChar}"
          }
      val color = movesResult.getString("color")
      moves.append("${color}[${positionCode}]")
    }

    val sgf_file_contents =
        "(;FF[4]CA[UTF-8]${comment}GM[1]${name}HA[${handicap}]KM[${komi}]PB[${blackPlayer}]PW[${whitePlayer}]RE[${winner}]SZ[${boardSize}];${moves})"
    val formatter =
        DateTimeFormatter.ofPattern("YYYY_MM_dd")
            .withLocale(Locale.getDefault())
            .withZone(ZoneId.of("UTC"))
    val date_string = formatter.format(created.toInstant())
    val filename =
        if (!name.isEmpty()) "${name}_${date_string}.sgf"
        else "${blackPlayer}_vs_${whitePlayer}_${date_string}.sgf"

    return ResponseEntity.ok()
        .header("Content-Type", "application/x-go-sgf")
        .header("Content-Disposition", "attachment; filename=\"${filename}\"")
        .body(sgf_file_contents)
  }
}
