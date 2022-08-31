import pytest

from record.go import Board, IllegalMoveException, Stone


@pytest.fixture
def board(size=9):
    return Board(size)


@pytest.mark.parametrize(
    ['point', 'adjacents'],
    [
        [(0, 0), [(1, 0), (0, 1)]],
        [(1, 0), [(0, 0), (2, 0), (1, 1)]],
        [(0, 1), [(1, 1), (0, 0), (0, 2)]],
        [(1, 1), [(0, 1), (2, 1), (1, 0), (1, 2)]],
        [(8, 8), [(7, 8), (8, 7)]],
    ],
    ids=['1-1', '2-1', '1-2', '2-2', '9-9'],
)
def test_adjacents(board, point, adjacents):
    assert list(board.adjacents(point)) == adjacents


def test_play_black_stone(board):
    board.place_stone(Stone.BLACK, 3, 3)
    assert board.moves == {(3, 3): Stone.BLACK}
    assert board.group_index == {(3, 3): {(3, 3)}}
    assert board.groups == {frozenset([(3, 3)])}


def test_play_two_black_stones(board):
    board.place_stone(Stone.BLACK, 3, 3)
    board.place_stone(Stone.BLACK, 3, 2)
    assert board.moves == {(3, 3): Stone.BLACK, (3, 2): Stone.BLACK}
    group = {(3, 3), (3, 2)}
    assert board.group_index == {(3, 3): group, (3, 2): group}
    assert board.group_index[(3, 2)] is board.group_index[(3, 3)]
    assert board.groups == {frozenset([(3, 2), (3, 3)])}


def test_play_four_black_stones(board):
    board.place_stone(Stone.BLACK, 0, 0)
    board.place_stone(Stone.BLACK, 0, 1)
    board.place_stone(Stone.BLACK, 1, 0)
    board.place_stone(Stone.BLACK, 1, 1)
    assert board.moves == {
        (0, 0): Stone.BLACK,
        (0, 1): Stone.BLACK,
        (1, 0): Stone.BLACK,
        (1, 1): Stone.BLACK,
    }
    group = {(0, 0), (0, 1), (1, 0), (1, 1)}
    assert board.group_index == {(0, 0): group, (0, 1): group, (1, 0): group, (1, 1): group}
    assert board.group_index[(0, 0)] is board.group_index[(1, 1)]
    assert board.groups == {frozenset([(0, 0), (0, 1), (1, 0), (1, 1)])}


def test_play_two_turns(board):
    board.place_stone(Stone.BLACK, 0, 0)
    board.place_stone(Stone.WHITE, 0, 1)
    assert board.moves == {(0, 0): Stone.BLACK, (0, 1): Stone.WHITE}
    assert board.group_index == {(0, 0): {(0, 0)}, (0, 1): {(0, 1)}}
    assert board.groups == {frozenset([(0, 0)]), frozenset([(0, 1)])}


def test_capture_corner(board):
    board.place_stone(Stone.WHITE, 0, 0)
    board.place_stone(Stone.BLACK, 0, 1)
    board.place_stone(Stone.BLACK, 1, 1)
    board.place_stone(Stone.BLACK, 1, 0)
    assert board.moves == {
        (0, 1): Stone.BLACK,
        (1, 1): Stone.BLACK,
        (1, 0): Stone.BLACK,
    }
    group = {(0, 1), (1, 1), (1, 0)}
    assert board.group_index == {(0, 1): group, (1, 1): group, (1, 0): group}
    assert board.groups == {frozenset(group)}


def test_capture_middle(board):
    board.place_stone(Stone.BLACK, 1, 1)
    board.place_stone(Stone.WHITE, 0, 1)
    board.place_stone(Stone.WHITE, 2, 1)
    board.place_stone(Stone.WHITE, 1, 0)
    board.place_stone(Stone.WHITE, 1, 2)
    assert board.moves == {
        (0, 1): Stone.WHITE,
        (2, 1): Stone.WHITE,
        (1, 0): Stone.WHITE,
        (1, 2): Stone.WHITE,
    }
    assert board.group_index == {
        (0, 1): {(0, 1)},
        (2, 1): {(2, 1)},
        (1, 0): {(1, 0)},
        (1, 2): {(1, 2)},
    }
    assert board.groups == {
        frozenset({(0, 1)}),
        frozenset({(2, 1)}),
        frozenset({(1, 0)}),
        frozenset({(1, 2)}),
    }


def test_existing_stone(board):
    board.place_stone(Stone.BLACK, 0, 0)
    with pytest.raises(IllegalMoveException, match='space is already occupied'):
        board.place_stone(Stone.BLACK, 0, 0)
    with pytest.raises(IllegalMoveException, match='space is already occupied'):
        board.place_stone(Stone.WHITE, 0, 0)


def test_suicide(board):
    board.place_stone(Stone.BLACK, 0, 1)
    board.place_stone(Stone.BLACK, 1, 0)
    with pytest.raises(IllegalMoveException, match='move is suicidal'):
        board.place_stone(Stone.WHITE, 0, 0)
