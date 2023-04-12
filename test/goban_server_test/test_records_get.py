from anys import ANY_DATETIME_STR


def test_get_record(user_client, user, record_factory, move_factory):
    record1 = record_factory()
    record2 = record_factory(board_size=19)
    response = user_client.get(f"/api/records/{record1['id']}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record1["id"],
        "owner": user["id"],
        "board_size": 9,
        "created": ANY_DATETIME_STR,
        "name": "",
        "black_player": "Black",
        "white_player": "White",
        "comment": "",
        "handicap": 0,
        "komi": 7.5,
        "ruleset": "AGA",
        "winner": "U",
        "moves": [],
        "stones": [],
    }

    response = user_client.get(f"/api/records/{record2['id']}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record2["id"],
        "owner": user["id"],
        "board_size": 19,
        "created": ANY_DATETIME_STR,
        "name": "",
        "black_player": "Black",
        "white_player": "White",
        "comment": "",
        "handicap": 0,
        "komi": 7.5,
        "ruleset": "AGA",
        "winner": "U",
        "moves": [],
        "stones": [],
    }

    # Verify getting a nonexistent record returns a 404
    response = user_client.get("/api/records/999/")
    assert response.status_code == 404

    # Verify that two moves are returned correctly
    move_factory(1, 0, record=record1)
    move_factory(0, 0, record=record1)
    response = user_client.get(f"/api/records/{record1['id']}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record1["id"],
        "owner": user["id"],
        "board_size": 9,
        "created": ANY_DATETIME_STR,
        "name": "",
        "black_player": "Black",
        "white_player": "White",
        "comment": "",
        "handicap": 0,
        "komi": 7.5,
        "ruleset": "AGA",
        "winner": "U",
        "stones": [
            {"x": 0, "y": 0, "color": "W"},
            {"x": 1, "y": 0, "color": "B"},
        ],
        "moves": [
            {"position": {"x": 1, "y": 0}, "color": "B", "captures": []},
            {"position": {"x": 0, "y": 0}, "color": "W", "captures": []},
        ],
    }

    # Verify that a capture is reflected in the moves field
    move_factory(0, 1, record=record1)
    response = user_client.get(f"/api/records/{record1['id']}/")
    assert response.status_code == 200
    assert response.json() == {
        "id": record1["id"],
        "owner": user["id"],
        "board_size": 9,
        "created": ANY_DATETIME_STR,
        "name": "",
        "black_player": "Black",
        "white_player": "White",
        "comment": "",
        "handicap": 0,
        "komi": 7.5,
        "ruleset": "AGA",
        "winner": "U",
        "stones": [
            {"x": 1, "y": 0, "color": "B"},
            {"x": 0, "y": 1, "color": "B"},
        ],
        "moves": [
            {"position": {"x": 1, "y": 0}, "color": "B", "captures": []},
            {"position": {"x": 0, "y": 0}, "color": "W", "captures": []},
            {
                "position": {"x": 0, "y": 1},
                "color": "B",
                "captures": [{"x": 0, "y": 0}],
            },
        ],
    }
