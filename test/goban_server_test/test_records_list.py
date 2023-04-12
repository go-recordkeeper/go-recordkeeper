from anys import ANY_DATETIME_STR


def test_list_records(user_client, user, record_factory):
    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == {"count": 0, "pages": 1, "results": []}

    record1 = record_factory()
    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == {
        "count": 1,
        "pages": 1,
        "results": [
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
        ],
    }

    record2 = record_factory(board_size=19)
    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 1,
        "results": [
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
        ],
    }

    response = user_client.get("/api/records/?page_size=1")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 2,
        "results": [
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
        ],
    }

    response = user_client.get("/api/records/?page_size=1&page=1")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 2,
        "results": [
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
        ],
    }

    response = user_client.get("/api/records/?page_size=1&page=2")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 2,
        "results": [
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
        ],
    }
