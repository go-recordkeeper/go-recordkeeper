import time
from subprocess import run
from urllib.parse import urljoin

import pytest
import requests


@pytest.fixture(scope="session", autouse=True)
def init_db():
    run(["docker", "compose", "down"])
    run(["docker", "compose", "up", "postgres", "-d", "--wait"])
    run(["poetry", "run", "python", "../server/django/manage.py", "migrate"])
    flush = run(["poetry", "run", "python", "../server/django/manage.py",
                "sqlflush"], capture_output=True).stdout
    reset_sequence = run(["poetry", "run", "python", "../server/django/manage.py",
                         "sqlsequencereset", "record"], capture_output=True).stdout
    with open('reset.sql', 'wb') as sql_file:
        sql_file.write(flush)
        sql_file.write(reset_sequence)
    yield
    run(["docker", "compose", "stop"])


@pytest.fixture(scope="function", autouse=True)
def clean_db():
    run(["psql", "--file", "reset.sql", "-h", "localhost",
        "default", "postgres"], env={"PGPASSWORD": "postgres"})
    yield


@pytest.fixture(scope="session", params=["django", "fastapi", "haskell"])
def impl(request):
    return request.param


@pytest.fixture(scope="session")
def server_under_test(impl):
    run(["docker", "compose", "--profile", impl, "up", "-d", "--wait"])
    # Wait for the service to be ready to receive requests.
    # This should be a healthcheck or something, but hey
    time.sleep(1)
    yield
    run(["docker", "compose", "--profile", impl, "stop", impl])
    run(["docker", "compose", "--profile", impl, "rm", "--force", impl])


class LocalhostSession(requests.Session):
    def __init__(self):
        super().__init__()
        self.base_url = "http://localhost:8000"

    def request(self, method, url, *args, **kwargs):
        url = urljoin(self.base_url, url)
        return super().request(method, url, *args, **kwargs)


@pytest.fixture
def client(server_under_test):
    return LocalhostSession()


@pytest.fixture
def user_client_factory(server_under_test):
    def factory(user):
        client = LocalhostSession()
        response = client.post(
            "/api/login/",
            json={"username": user["username"], "password": user["password"]},
        )
        assert response.status_code == 200
        token = response.json()
        client.headers["Authorization"] = f"Bearer {token}"
        return client

    return factory


@pytest.fixture
def user_client(user_client_factory, user):
    return user_client_factory(user)


@pytest.fixture
def user_factory(client, faker):
    def factory(username=None, email=None, password=None):
        if username is None:
            username = faker.first_name()
        if email is None:
            email = faker.email()
        if password is None:
            password = faker.password()
        return {
            **client.post(
                "/api/register/",
                json={"username": username,
                      "email": email, "password": password},
            ).json(),
            "password": password,
        }

    return factory


@pytest.fixture
def user(user_factory):
    return user_factory()


@pytest.fixture
def record_factory(user_client_factory, user):
    def factory(owner=None, board_size=9, handicap=0):
        if owner is None:
            owner = user
        user_client = user_client_factory(owner)
        record = user_client.post(
            "/api/records/",
            json={"board_size": board_size, "handicap": handicap}
        ).json()
        return record

    return factory


@pytest.fixture
def record(record_factory):
    return record_factory()


@pytest.fixture
def move_factory(user_client, record):
    default_record = record

    def factory(x, y, record=None):
        if record is None:
            record = default_record
        move = user_client.post(
            f"/api/records/{record['id']}/play/",
            json={"x": x, "y": y}
        ).json()
        return move

    return factory
