from sys import breakpointhook
import pytest


def test_login(client, user_factory, faker):
    password = faker.password()
    user = user_factory(password=password)
    response = client.post(
        "/api/login/", json={"username": user["username"], "password": user["password"]}
    )
    assert response.status_code == 200
    token = response.json()
    assert token


def test_get_records(client):
    response = client.get("/api/records/")
    print(response)
    assert response.status_code == 403

