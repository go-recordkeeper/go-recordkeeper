from anys import ANY_DATETIME_STR, ANY_INT


def test_get_records(user_client, user, record_factory):
    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == []

    record1 = record_factory()
    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == [
        {
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
        }
    ]

    record2 = record_factory(board_size=19)
    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == [
        {
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
        },
        {
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
        },
    ]


def test_get_record(user_client, user, record_factory):
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

    response = user_client.get("/api/records/999/")
    assert response.status_code == 404


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


def test_update_record(user_client, user, record):
    update_response = user_client.put(
        f"/api/records/{record['id']}/",
        json={
            "board_size": 19,
            "name": "Kickass Game",
            "black_player": "Preto",
            "white_player": "Branco",
            "comment": "tee hee",
            "handicap": 6,
            "komi": 0,
            "ruleset": "JPN",
            "winner": "B",
        },
    )
    assert update_response.status_code == 200
    assert update_response.json() == {
        "id": record["id"],
        "owner": user["id"],
        "board_size": 9,  # Board size is immutable
        "created": ANY_DATETIME_STR,
        "name": "Kickass Game",
        "black_player": "Preto",
        "white_player": "Branco",
        "comment": "tee hee",
        "handicap": 6,
        "komi": 0,
        "ruleset": "JPN",
        "winner": "B",
    }

    # Verify changes were persisted to the database
    get_response = user_client.get(f"/api/records/{record['id']}/")
    assert get_response.status_code == 200
    assert get_response.json() == {
        **update_response.json(),
        "moves": [],
        "stones": [],
    }
