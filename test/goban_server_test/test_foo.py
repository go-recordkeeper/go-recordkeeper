import pytest


def test_get_user(client, impl):
    response = client.get("/api/user/")
    print(response)
    assert response.status_code == 1234


def test_get_records(client, impl):
    response = client.get("/api/records/")
    print(response)
    assert response.status_code == 1234
