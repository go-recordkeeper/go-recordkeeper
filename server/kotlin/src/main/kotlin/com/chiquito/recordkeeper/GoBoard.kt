package com.chiquito.recordkeeper

import java.util.Optional

enum class Color {
  B {
    override fun invert() = W
  },
  W {
    override fun invert() = B
  };

  abstract fun invert(): Color
}

class GoBoard(size: Int) {
  val size = size
  val stones = HashMap<Position, Color>()
  val moves = ArrayList<Move>()

  // index and (x,y) are technically redundant, but it's very convenient to have both handy
  data class Position(val index: Int, val x: Int, val y: Int)
  fun fromCoordinates(x: Int, y: Int): Position = Position(x + (y * size), x, y)
  fun fromIndex(index: Int): Position = Position(index, index % size, index / size)
  fun adjacents(pos: Position) = sequence {
    if (pos.x > 0) yield(fromCoordinates(pos.x - 1, pos.y))
    if (pos.x < size - 1) yield(fromCoordinates(pos.x + 1, pos.y))
    if (pos.y > 0) yield(fromCoordinates(pos.x, pos.y - 1))
    if (pos.y < size - 1) yield(fromCoordinates(pos.x, pos.y + 1))
  }

  data class Move(val pos: Optional<Position>, val color: Color)

  data class Group(val color: Color, val stones: Set<Position>, val liberties: Set<Position>) {
    companion object {
      fun build(board: GoBoard, seed: Position): Optional<Group> {
        val color = board.stones.get(seed)
        if (color == null) {
          return Optional.empty()
        }
        val queue: ArrayList<Position> = ArrayList()
        queue.add(seed)
        val stones: HashSet<Position> = HashSet()
        val liberties: HashSet<Position> = HashSet()
        while (!queue.isEmpty()) {
          val pos = queue.removeLast()
          val posColor = board.stones.get(pos)
          if (posColor == null) {
            liberties.add(pos)
          } else if (posColor == color) {
            stones.add(pos)
            board.adjacents(pos).forEach {
              if (!stones.contains(it) && !liberties.contains(it) && !queue.contains(it)) {
                queue.add(it)
              }
            }
          }
        }
        return Optional.of(Group(color, stones, liberties))
      }
    }
  }
}
