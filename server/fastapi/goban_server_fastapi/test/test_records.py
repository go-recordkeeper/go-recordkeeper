from anys import ANY_DATETIME_STR


def test_get_records(user_client, record_factory):
    record = record_factory()

    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == [
        {
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
        }
    ]


def test_get_two_records(user_client, record_factory):
    record1 = record_factory()
    record2 = record_factory()

    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == [
        {
            "id": record2.id,
            "owner": record2.owner_id,
            "board_size": record2.board_size,
            "created": ANY_DATETIME_STR,
            "name": record2.name,
            "black_player": record2.black_player,
            "white_player": record2.white_player,
            "comment": record2.comment,
            "handicap": record2.handicap,
            "komi": record2.komi,
            "ruleset": record2.ruleset,
            "winner": record2.winner,
        },
        {
            "id": record1.id,
            "owner": record1.owner_id,
            "board_size": record1.board_size,
            "created": ANY_DATETIME_STR,
            "name": record1.name,
            "black_player": record1.black_player,
            "white_player": record1.white_player,
            "comment": record1.comment,
            "handicap": record1.handicap,
            "komi": record1.komi,
            "ruleset": record1.ruleset,
            "winner": record1.winner,
        },
    ]


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
        "moves": [{"position": 0, "color": "B", "captures": []}],
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
            {"position": 0, "color": "B", "captures": []},
            {"position": 1, "color": "W", "captures": []},
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
            {"position": 1, "color": "B", "captures": []},
            {"position": 0, "color": "W", "captures": []},
            {"position": 19, "color": "B", "captures": [{"x": 0, "y": 0}]},
        ],
    }


def test_get_record_does_not_exist(user_client, record):
    response = user_client.get(f"/api/records/9999/")
    assert response.status_code == 404
