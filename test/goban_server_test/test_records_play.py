import pytest
from anys import ANY_DATETIME_STR


def test_play_move(user_client, record):
    response = user_client.post(
        f"/api/records/{record['id']}/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 201
    assert response.json() == {
        "add": [{"x": 0, "y": 0, "color": "B"}],
        "remove": [],
    }

    # Verify changes were persisted to the database
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 200
    assert response.json()["stones"] == [{"x": 0, "y": 0, "color": "B"}]
    assert response.json()["moves"] == [{"position": 0, "color": "B", "captures": []}]


def test_play_capture(user_client, record):
    # Play two stones like this:
    # W B
    # . .
    response = user_client.post(
        f"/api/records/{record['id']}/play/",
        json={"x": 1, "y": 0},
    )
    assert response.status_code == 201
    assert response.json() == {
        "add": [{"x": 1, "y": 0, "color": "B"}],
        "remove": [],
    }
    response = user_client.post(
        f"/api/records/{record['id']}/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 201
    assert response.json() == {
        "add": [{"x": 0, "y": 0, "color": "W"}],
        "remove": [],
    }

    # Play a black stone to capture the white stone
    response = user_client.post(
        f"/api/records/{record['id']}/play/",
        json={"x": 0, "y": 1},
    )
    assert response.status_code == 201
    assert response.json() == {
        "add": [{"x": 0, "y": 1, "color": "B"}],
        "remove": [{"x": 0, "y": 0}],
    }


@pytest.mark.parametrize(
    ["handicap", "colors"],
    [
        (0, ["B", "W", "B", "W", "B"]),
        (1, ["B", "W", "B", "W", "B"]),
        (2, ["B", "B", "W", "B", "W"]),
        (3, ["B", "B", "B", "W", "B"]),
    ],
    ids=["0", "1", "2", "3"],
)
def test_play_with_handicap(user_client, record_factory, handicap, colors):
    record = record_factory(handicap=handicap)
    for i in range(0, 5):
        response = user_client.post(
            f"/api/records/{record['id']}/play/",
            json={"x": i, "y": 0},
        )
        assert response.status_code == 201

    # Verify that the colors are in the correct order
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 200
    assert [stone["color"] for stone in response.json()["stones"]] == colors
    assert [move["color"] for move in response.json()["moves"]] == colors


def test_play_errors(user_client, record, move_factory):
    # Set up this board state:
    # . B
    # B W
    move_factory(0, 1)
    move_factory(1, 1)
    move_factory(1, 0)

    # Verify that playing a stone on top of another one is not allowed
    response = user_client.post(
        f"/api/records/{record['id']}/play/",
        json={"x": 1, "y": 1},
    )
    assert response.status_code == 403

    # Verify that playing a white stone in the corner is suicidal
    response = user_client.post(
        f"/api/records/{record['id']}/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 403

    # Verify that playing a move on a nonexistent record isn't allowed
    response = user_client.post(
        f"/api/records/99999/play/",
        json={"x": 0, "y": 0},
    )
    assert response.status_code == 404
