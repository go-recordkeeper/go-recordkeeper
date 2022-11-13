import pytest
from sqlalchemy import select

from goban_server_fastapi.records.models import Move


def test_play_move(db, user_client, record):
    response = user_client.post(
        f"/api/records/{record.id}/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 200
    assert response.json() == {
        "add": [{"x": 0, "y": 0, "color": "B"}],
        "remove": [],
    }
    with db.session() as session:
        moves = list(session.scalars(select(Move).where(Move.record_id == record.id)))
        assert len(moves) == 1
        move = moves[0]
        assert move.position == 0
        assert move.move == 1
        assert move.color == "B"


@pytest.mark.parametrize(
    ["handicap", "colors"],
    [
        (0, ["B", "W", "B", "W", "B"]),
        (1, ["B", "W", "B", "W", "B"]),
        (2, ["B", "B", "W", "B", "W"]),
        (3, ["B", "B", "B", "W", "B"]),
    ],
    ids=["0", "1", "2", "3"],
)
def test_play_moves_with_handicap(db, user_client, record_factory, handicap, colors):
    record = record_factory(handicap=handicap)
    for i in range(0, 5):
        response = user_client.post(
            f"/api/records/{record.id}/play/",
            json={"x": i, "y": 0},
        )
        assert response.status_code == 200
    with db.session() as session:
        moves = list(session.scalars(select(Move).where(Move.record_id == record.id)))
        assert len(moves) == 5
        for i, move in enumerate(moves):
            assert move.position == i
            assert move.move == i + 1
        assert [move.color for move in moves] == colors


def test_play_capture(user_client, record, move_factory):
    move_factory(1, 0, "B")
    move_factory(0, 0, "W")
    response = user_client.post(
        f"/api/records/{record.id}/play/",
        json={"x": 0, "y": 1},
    )
    assert response.status_code == 200
    assert response.json() == {
        "add": [{"x": 0, "y": 1, "color": "B"}],
        "remove": [{"x": 0, "y": 0}],
    }


def test_play_capture_group(user_client, record, move_factory):
    move_factory(1, 0, "B")
    move_factory(0, 0, "W")
    move_factory(1, 1, "B")
    move_factory(0, 1, "W")
    move_factory(1, 2, "B")
    move_factory(0, 2, "W")
    response = user_client.post(
        f"/api/records/{record.id}/play/",
        json={"x": 0, "y": 3},
    )
    assert response.status_code == 200
    assert response.json() == {
        "add": [{"x": 0, "y": 3, "color": "B"}],
        "remove": [{"x": 0, "y": 0}, {"x": 0, "y": 1}, {"x": 0, "y": 2}],
    }


def test_play_over_existing_stone(user_client, record, move_factory):
    move_factory(0, 0, "B")
    response = user_client.post(
        f"/api/records/{record.id}/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 403


def test_play_suicidal_move(user_client, record, move_factory):
    move_factory(1, 0, "B")
    move_factory(1, 1, "W")
    move_factory(0, 1, "B")
    response = user_client.post(
        f"/api/records/{record.id}/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 403
