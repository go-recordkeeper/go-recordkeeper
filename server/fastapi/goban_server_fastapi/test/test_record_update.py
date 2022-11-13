import pytest
from anys import ANY_DATETIME_STR, ANY_INT


def test_update_record(user_client, record, user, faker):
    board_size = 9
    name = faker.sentence()
    black_player = faker.first_name()
    white_player = faker.first_name()
    comment = faker.sentence()
    handicap = 1
    komi = 7.5
    ruleset = "AGA"
    winner = "B"
    response = user_client.put(
        f"/api/records/{record.id}/",
        json={
            "board_size": board_size,
            "name": name,
            "black_player": black_player,
            "white_player": white_player,
            "comment": comment,
            "handicap": handicap,
            "komi": komi,
            "ruleset": ruleset,
            "winner": winner,
        },
    )
    assert response.status_code == 200
    assert response.json() == {
        "id": record.id,
        "owner": user.id,
        "board_size": 19,  # Updating board size is not allowed
        "created": ANY_DATETIME_STR,
        "name": name,
        "black_player": black_player,
        "white_player": white_player,
        "comment": comment,
        "handicap": handicap,
        "komi": komi,
        "ruleset": ruleset,
        "winner": "B",
    }
