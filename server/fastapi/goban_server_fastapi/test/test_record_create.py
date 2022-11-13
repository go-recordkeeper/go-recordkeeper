import pytest
from anys import ANY_DATETIME_STR, ANY_INT


@pytest.mark.parametrize("board_size", [9, 13, 19], ids=["9x9", "13x13", "19x19"])
def test_create_sparse_record(user_client, user, faker, board_size):
    response = user_client.post("/api/records/", json={"board_size": board_size})
    assert response.status_code == 201
    assert response.json() == {
        "id": ANY_INT,
        "owner": user.id,
        "board_size": board_size,
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


def test_create_record(user_client, user, faker):
    board_size = 19
    name = faker.sentence()
    black_player = faker.first_name()
    white_player = faker.first_name()
    comment = faker.sentence()
    handicap = 1
    komi = 7.5
    ruleset = "AGA"
    response = user_client.post(
        "/api/records/",
        json={
            "board_size": board_size,
            "name": name,
            "black_player": black_player,
            "white_player": white_player,
            "comment": comment,
            "handicap": handicap,
            "komi": komi,
            "ruleset": ruleset,
        },
    )
    assert response.status_code == 201
    assert response.json() == {
        "id": ANY_INT,
        "owner": user.id,
        "board_size": board_size,
        "created": ANY_DATETIME_STR,
        "name": name,
        "black_player": black_player,
        "white_player": white_player,
        "comment": comment,
        "handicap": handicap,
        "komi": komi,
        "ruleset": ruleset,
        "winner": "U",
    }
