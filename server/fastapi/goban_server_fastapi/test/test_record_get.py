import pytest
from anys import ANY_DATETIME_STR, ANY_INT


def test_get_empty_record(user_client, record):
    response = user_client.get(f"/api/records/{record.id}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record.id,
        "owner": record.owner_id,
        "board_size": record.board_size,
        "created": ANY_DATETIME_STR,
        "name": record.name,
        "black_player": record.black_player,
        "white_player": record.white_player,
        "comment": record.comment,
        "handicap": record.handicap,
        "komi": record.komi,
        "ruleset": record.ruleset,
        "winner": record.winner,
        "stones": [],
        "moves": [],
    }


def test_get_record_one_move(user_client, record, move_factory):
    move_factory(0, 0, "B")
    response = user_client.get(f"/api/records/{record.id}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record.id,
        "owner": record.owner_id,
        "board_size": record.board_size,
        "created": ANY_DATETIME_STR,
        "name": record.name,
        "black_player": record.black_player,
        "white_player": record.white_player,
        "comment": record.comment,
        "handicap": record.handicap,
        "komi": record.komi,
        "ruleset": record.ruleset,
        "winner": record.winner,
        "stones": [{"x": 0, "y": 0, "color": "B"}],
        "moves": [{"position": {"x": 0, "y": 0}, "color": "B", "captures": []}],
    }


def test_get_record_two_moves(user_client, record, move_factory):
    move_factory(0, 0, "B")
    move_factory(1, 0, "W")
    response = user_client.get(f"/api/records/{record.id}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record.id,
        "owner": record.owner_id,
        "board_size": record.board_size,
        "created": ANY_DATETIME_STR,
        "name": record.name,
        "black_player": record.black_player,
        "white_player": record.white_player,
        "comment": record.comment,
        "handicap": record.handicap,
        "komi": record.komi,
        "ruleset": record.ruleset,
        "winner": record.winner,
        "stones": [
            {"x": 0, "y": 0, "color": "B"},
            {"x": 1, "y": 0, "color": "W"},
        ],
        "moves": [
            {"position": {"x": 0, "y": 0}, "color": "B", "captures": []},
            {"position": {"x": 1, "y": 0}, "color": "W", "captures": []},
        ],
    }


def test_get_record_capture(user_client, record, move_factory):
    move_factory(1, 0, "B")
    move_factory(0, 0, "W")
    move_factory(0, 1, "B")
    response = user_client.get(f"/api/records/{record.id}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record.id,
        "owner": record.owner_id,
        "board_size": record.board_size,
        "created": ANY_DATETIME_STR,
        "name": record.name,
        "black_player": record.black_player,
        "white_player": record.white_player,
        "comment": record.comment,
        "handicap": record.handicap,
        "komi": record.komi,
        "ruleset": record.ruleset,
        "winner": record.winner,
        "stones": [
            {"x": 1, "y": 0, "color": "B"},
            {"x": 0, "y": 1, "color": "B"},
        ],
        "moves": [
            {"position": {"x": 1, "y": 0}, "color": "B", "captures": []},
            {"position": {"x": 0, "y": 0}, "color": "W", "captures": []},
            {"position": {"x": 0, "y": 1}, "color": "B",
                "captures": [{"x": 0, "y": 0}]},
        ],
    }


def test_get_record_one_pass(user_client, record, move_factory):
    user_client.post(f"/api/records/{record.id}/pass/")
    response = user_client.get(f"/api/records/{record.id}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record.id,
        "owner": record.owner_id,
        "board_size": record.board_size,
        "created": ANY_DATETIME_STR,
        "name": record.name,
        "black_player": record.black_player,
        "white_player": record.white_player,
        "comment": record.comment,
        "handicap": record.handicap,
        "komi": record.komi,
        "ruleset": record.ruleset,
        "winner": record.winner,
        "stones": [],
        "moves": [{"position": None, "color": "B", "captures": []}],
    }


def test_get_record_does_not_exist(user_client, record):
    response = user_client.get("/api/records/9999/")
    assert response.status_code == 404
