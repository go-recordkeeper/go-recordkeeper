import pytest
from anys import ANY_DATETIME_STR, ANY_INT


def test_list_records(user_client, record_factory):
    record = record_factory()

    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == {
        "count": 1,
        "pages": 1,
        "results": [
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
        ],
    }


def test_list_two_records(user_client, record_factory):
    record1 = record_factory()
    record2 = record_factory()

    response = user_client.get("/api/records/")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 1,
        "results": [
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
        ],
    }


def test_list_two_records_one_page(user_client, record_factory):
    record1 = record_factory()
    record2 = record_factory()

    response = user_client.get("/api/records/?page_size=1&page=1")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 2,
        "results": [
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
        ],
    }

    response = user_client.get("/api/records/?page_size=1&page=2")
    assert response.status_code == 200
    assert response.json() == {
        "count": 2,
        "pages": 2,
        "results": [
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
        ],
    }
