package com.chiquito.recordkeeper

import org.assertj.core.api.Assertions.*
import org.junit.jupiter.api.Test

class GoBoardTests {

  // @ParameterizedTest
  // @ValueSource(ints = [9, 13, 19])
  // fun contextLoads(boardSize: Int) {
  //   assertThat(boardSize).isEqualTo(19)
  // }

  @Test
  fun createPosition() {
    val board = GoBoard(9)
    assertThat(board.fromCoordinates(0, 0)).isEqualTo(board.fromIndex(0))
    assertThat(board.fromCoordinates(1, 0)).isEqualTo(board.fromIndex(1))
    assertThat(board.fromCoordinates(0, 1)).isEqualTo(board.fromIndex(9))
    assertThat(board.fromCoordinates(1, 1)).isEqualTo(board.fromIndex(10))
  }

  @Test
  fun adjacents() {
    val board = GoBoard(9)
    assertThat(board.adjacents(board.fromCoordinates(0, 0)).toList())
        .isEqualTo(listOf(board.fromCoordinates(1, 0), board.fromCoordinates(0, 1)))
    assertThat(board.adjacents(board.fromCoordinates(1, 0)).toList())
        .isEqualTo(
            listOf(
                board.fromCoordinates(0, 0),
                board.fromCoordinates(2, 0),
                board.fromCoordinates(1, 1)
            )
        )
    assertThat(board.adjacents(board.fromCoordinates(0, 1)).toList())
        .isEqualTo(
            listOf(
                board.fromCoordinates(1, 1),
                board.fromCoordinates(0, 0),
                board.fromCoordinates(0, 2)
            )
        )
    assertThat(board.adjacents(board.fromCoordinates(1, 1)).toList())
        .isEqualTo(
            listOf(
                board.fromCoordinates(0, 1),
                board.fromCoordinates(2, 1),
                board.fromCoordinates(1, 0),
                board.fromCoordinates(1, 2)
            )
        )
  }

  @Test
  fun buildGroup() {
    val board = GoBoard(9)
    assertThat(GoBoard.Group.build(board, board.fromCoordinates(0, 0))).isEmpty()
    // One black stone in the corner
    board.stones.set(board.fromCoordinates(0, 0), Color.B)
    assertThat(GoBoard.Group.build(board, board.fromCoordinates(0, 0)))
        .hasValue(
            GoBoard.Group(
                Color.B,
                setOf(board.fromCoordinates(0, 0)),
                setOf(board.fromCoordinates(1, 0), board.fromCoordinates(0, 1))
            )
        )
    // Another black stone
    board.stones.set(board.fromCoordinates(0, 1), Color.B)
    assertThat(GoBoard.Group.build(board, board.fromCoordinates(0, 0)))
        .hasValue(
            GoBoard.Group(
                Color.B,
                setOf(
                    board.fromCoordinates(0, 0),
                    board.fromCoordinates(0, 1),
                ),
                setOf(
                    board.fromCoordinates(1, 0),
                    board.fromCoordinates(1, 1),
                    board.fromCoordinates(0, 2),
                )
            )
        )
    // A third black stone to create a shared liberty at (1,0)
    board.stones.set(board.fromCoordinates(1, 1), Color.B)
    assertThat(GoBoard.Group.build(board, board.fromCoordinates(0, 0)))
        .hasValue(
            GoBoard.Group(
                Color.B,
                setOf(
                    board.fromCoordinates(0, 0),
                    board.fromCoordinates(0, 1),
                    board.fromCoordinates(1, 1),
                ),
                setOf(
                    board.fromCoordinates(1, 0),
                    board.fromCoordinates(2, 1),
                    board.fromCoordinates(0, 2),
                    board.fromCoordinates(1, 2),
                )
            )
        )
    // A white stone that occupies a liberty
    board.stones.set(board.fromCoordinates(1, 0), Color.W)
    assertThat(GoBoard.Group.build(board, board.fromCoordinates(0, 0)))
        .hasValue(
            GoBoard.Group(
                Color.B,
                setOf(
                    board.fromCoordinates(0, 0),
                    board.fromCoordinates(0, 1),
                    board.fromCoordinates(1, 1),
                ),
                setOf(
                    board.fromCoordinates(2, 1),
                    board.fromCoordinates(0, 2),
                    board.fromCoordinates(1, 2),
                )
            )
        )
  }
}
