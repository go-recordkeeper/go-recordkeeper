import pytest
from anys import ANY_DATETIME_STR


def test_undo_move(user_client, record, move_factory):
    move_factory(0, 0)
    response = user_client.post(f"/api/records/{record['id']}/undo/")
    assert response.status_code == 200
    assert response.json() == {
        "add": [],
        "remove": [{"x": 0, "y": 0}],
    }

    # Verify changes were persisted to the database
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 200
    assert response.json()["stones"] == []
    assert response.json()["moves"] == []

    # Verify that captures are undo'd correctly
    move_factory(1, 0)
    move_factory(0, 0)
    move_factory(0, 1)
    response = user_client.post(f"/api/records/{record['id']}/undo/")
    assert response.status_code == 200
    assert response.json() == {
        "add": [{"x": 0, "y": 0, "color": "W"}],
        "remove": [{"x": 0, "y": 1}],
    }

    # Verify changes were persisted to the database
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 200
    assert response.json()["stones"] == [
        {"x": 1, "y": 0, "color": "B"},
        {"x": 0, "y": 0, "color": "W"},
    ]
    assert response.json()["moves"] == [
        {"position": 1, "color": "B", "captures": []},
        {"position": 0, "color": "W", "captures": []},
    ]

    # Undo the remaining two moves
    user_client.post(f"/api/records/{record['id']}/undo/")
    user_client.post(f"/api/records/{record['id']}/undo/")

    # Verify that you can't undo when there are no moves
    response = user_client.post(f"/api/records/{record['id']}/undo/")
    assert response.status_code == 403
