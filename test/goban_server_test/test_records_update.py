from anys import ANY_DATETIME_STR


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
