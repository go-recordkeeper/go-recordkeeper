from anys import ANY_DATETIME_STR, ANY_INT


def test_create_sparse_record(user_client, user):
    response = user_client.post("/api/records/", json={"board_size": 9})
    assert response.status_code == 201
    assert response.json() == {
        "id": ANY_INT,
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
    }

    # Test the other valid sizes
    # TODO parametrize
    response = user_client.post("/api/records/", json={"board_size": 13})
    assert response.status_code == 201
    assert response.json() == {
        "id": ANY_INT,
        "owner": user["id"],
        "board_size": 13,
        "created": ANY_DATETIME_STR,
        "name": "",
        "black_player": "Black",
        "white_player": "White",
        "comment": "",
        "handicap": 0,
        "komi": 7.5,
        "ruleset": "AGA",
        "winner": "U",
    }

    response = user_client.post("/api/records/", json={"board_size": 19})
    assert response.status_code == 201
    assert response.json() == {
        "id": ANY_INT,
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
    }

    # Verify that invalid sizes are invalid
    response = user_client.post("/api/records/", json={"board_size": 10})
    assert response.status_code == 400


def test_create_record(user_client, user):
    response = user_client.post(
        "/api/records/",
        json={
            "board_size": 19,
            "name": "Kickass Game",
            "black_player": "Preto",
            "white_player": "Branco",
            "comment": "tee hee",
            "handicap": 6,
            "komi": 0,
            "ruleset": "JPN",
        },
    )
    assert response.status_code == 201
    assert response.json() == {
        "id": ANY_INT,
        "owner": user["id"],
        "board_size": 19,
        "created": ANY_DATETIME_STR,
        "name": "Kickass Game",
        "black_player": "Preto",
        "white_player": "Branco",
        "comment": "tee hee",
        "handicap": 6,
        "komi": 0.0,
        "ruleset": "JPN",
        "winner": "U",
    }
