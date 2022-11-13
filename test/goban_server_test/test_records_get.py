from anys import ANY_DATETIME_STR




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

