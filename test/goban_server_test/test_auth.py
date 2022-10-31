from sys import breakpointhook

import pytest
from anys import ANY_INT


def test_login(client, user_factory, faker):
    password = faker.password()
    user = user_factory(password=password)

    # Verify login succeeds with good credentials
    response = client.post(
        "/api/login/", json={"username": user["username"], "password": user["password"]}
    )
    assert response.status_code == 200
    token = response.json()
    assert token

    # Verify login fails with incorrect password
    response = client.post(
        "/api/login/",
        json={"username": user["username"], "password": user["password"] + "!"},
    )
    assert response.status_code == 401

    # Verify login fails when using the email instead of the username
    response = client.post(
        "/api/login/", json={"username": user["email"], "password": user["password"]}
    )
    assert response.status_code == 401


def test_register(client, faker):
    # Verify registration succeeds with valid inputs
    username = faker.first_name()
    email = faker.email()
    password = faker.password()
    response = client.post(
        "/api/register/",
        json={"username": username, "email": email, "password": password},
    )
    assert response.status_code == 201
    assert response.json() == {
        "username": username,
        "email": email,
        "id": ANY_INT,
    }

    # Verify duplicate username fails
    new_email = faker.email()
    assert email != new_email
    response = client.post(
        "/api/register/",
        json={"username": username, "email": new_email, "password": password},
    )
    assert response.status_code == 400

    # Verify duplicate password succeeds
    new_username = faker.first_name()
    assert username != new_username
    response = client.post(
        "/api/register/",
        json={"username": new_username, "email": new_email, "password": password},
    )
    assert response.status_code == 201
    assert response.json() == {
        "username": new_username,
        "email": new_email,
        "id": ANY_INT,
    }

    # Verify malformed email fails
    good_username = faker.first_name()
    bad_email = faker.name()
    response = client.post(
        "/api/register/",
        json={"username": good_username, "email": bad_email, "password": password},
    )
    assert response.status_code == 400


def test_get_user(user_client, client, user):
    response = user_client.get("/api/user/")
    assert response.status_code == 200
    assert response.json() == {
        "username": user["username"],
        "email": user["email"],
        "id": ANY_INT,
    }

    # Verify an unauthenticated client does not work
    response = client.get("/api/user/")
    assert response.status_code == 403


def test_auth_flow(client, faker, user):
    username = faker.first_name()
    email = faker.email()
    password = faker.password()

    # Register the new user
    response = client.post(
        "/api/register/",
        json={"username": username, "email": email, "password": password},
    )
    assert response.status_code == 201

    # Log in and fetch the token
    response = client.post(
        "/api/login/", json={"username": username, "password": password}
    )
    assert response.status_code == 200
    token = response.json()
    assert token

    # Verify that the newly created user can be fetched with the token
    response = client.get("/api/user/", headers={"Authorization": f"Bearer {token}"})
    assert response.status_code == 200
    assert response.json() == {
        "username": username,
        "email": email,
        "id": ANY_INT,
    }

    # Verify that the old user can also login and fetch
    response = client.post(
        "/api/login/", json={"username": user["username"], "password": user["password"]}
    )
    assert response.status_code == 200
    token = response.json()
    response = client.get("/api/user/", headers={"Authorization": f"Bearer {token}"})
    assert response.status_code == 200
    assert response.json() == {
        "username": user["username"],
        "email": user["email"],
        "id": ANY_INT,
    }
