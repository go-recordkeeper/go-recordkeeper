import pytest

from .conftest import LocalhostSession, implementations


@pytest.mark.parametrize(
    "impl2",
    implementations.keys(),
    ids=implementations.keys(),
)
def test_compatible_login(impl, client, user, impl2):
    print(f"Logging in with {impl}")
    response = client.post(
        "/api/login/",
        json={"username": user["username"], "password": user["password"]},
    )
    assert response.status_code == 200
    token = response.json()

    print(f"Creating client for {impl2}")
    client2 = LocalhostSession(impl2)
    client2.headers["Authorization"] = f"Bearer {token}"

    print("Listing records")
    response = client2.get("/api/records/")
    assert response.status_code == 200
