import pytest
from anys import ANY_DATETIME_STR


def test_pass_turn(user_client, record):
    response = user_client.post(f"/api/records/{record['id']}/pass/")
    assert response.status_code == 201

    # Verify changes were persisted to the database
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 200
    assert response.json()["stones"] == []
    assert response.json()["moves"] == [
        {"position": None, "color": "B", "captures": []}
    ]

    # Pass again, just to be safe
    response = user_client.post(f"/api/records/{record['id']}/pass/")
    assert response.status_code == 201

    # Verify changes were persisted to the database
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 200
    assert response.json()["stones"] == []
    assert response.json()["moves"] == [
        {"position": None, "color": "B", "captures": []},
        {"position": None, "color": "W", "captures": []},
    ]
