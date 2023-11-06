package com.chiquito.recordkeeper

import java.util.Optional

abstract class GoException : Exception()

class OutOfBoundsException : GoException()

class SpaceOccupiedException(val position: GoBoard.Position) : GoException()

class SuicideException(val position: GoBoard.Position) : GoException()

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
  fun fromCoordinates(x: Int, y: Int): Position {
    if (x < 0 || x >= size || y < 0 || y >= size) {
      throw OutOfBoundsException()
    }
    return Position(x + (y * size), x, y)
  }
  fun fromIndex(index: Int): Position {
    if (index < 0 || index >= (size * size)) {
      throw OutOfBoundsException()
    }
    return Position(index, index % size, index / size)
  }

  fun adjacents(pos: Position) = sequence {
    if (pos.x > 0) yield(fromCoordinates(pos.x - 1, pos.y))
    if (pos.x < size - 1) yield(fromCoordinates(pos.x + 1, pos.y))
    if (pos.y > 0) yield(fromCoordinates(pos.x, pos.y - 1))
    if (pos.y < size - 1) yield(fromCoordinates(pos.x, pos.y + 1))
  }

  data class Move(val color: Color, val pos: Optional<Position>)

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

  fun placeStone(color: Color, pos: Position): List<Position> {
    if (stones.get(pos) != null) {
      throw SpaceOccupiedException(pos)
    }
    // Provisionally place the stone so that we can do group calculations
    stones.set(pos, color)
    // Check adjacent positions for dead groups
    val capturedStones = HashSet<Position>()
    adjacents(pos).forEach {
      val group = Group.build(this, it)
      // If there is a group adjacent, of the opposite color, and with no liberties, kill it
      if (!group.isEmpty() && group.get().color == color.invert() && group.get().liberties.size == 0
      ) {
        group.get().stones.forEach {
          stones.remove(it)
          capturedStones.add(it)
        }
      }
    }
    // Check if the original move was actually suicidal
    if (Group.build(this, pos).get().liberties.size == 0) {
      // Remove the suicidal move from the board
      stones.remove(pos)
      // We do not need to remove any adjacent kills, because they would make this move not actually
      // suicidal.
      throw SuicideException(pos)
    }
    return capturedStones.sortedBy { it.index }
  }
}
