from subprocess import run
from urllib.parse import urljoin

import pytest
import requests


@pytest.fixture(scope="session", autouse=True)
def init_db():
    run(["docker", "compose", "down"])
    run(
        ["docker", "compose", "run", "--rm", "django", "python", "manage.py", "migrate"]
    )
    yield
    run(["docker", "compose", "stop"])


@pytest.fixture(scope="function", autouse=True)
def clean_db():
    run(
        [
            "docker",
            "compose",
            "run",
            "--rm",
            "django",
            "python",
            "manage.py",
            "reset_db",
            "-c",
            "--noinput",
        ]
    )
    run(
        ["docker", "compose", "run", "--rm", "django", "python", "manage.py", "migrate"]
    )
    yield


@pytest.fixture(scope="session", params=["django", "fastapi"])
def impl(request):
    return request.param


@pytest.fixture(scope="session")
def server_under_test(impl):
    run(["docker", "compose", "--profile", impl, "up", "-d", "--wait"])
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
def django_command():
    def _command(command):
        resp = run(
            [
                "docker",
                "compose",
                "run",
                "--rm",
                "django",
                "python",
                "manage.py",
                "shell_plus",
                "-c",
                f"{command}",
            ],
            capture_output=True,
        )
        # There is a lot of import information logged, so attempt to trim it out before returning output
        return resp.stdout.split(b"\n")[-2]

    return _command


@pytest.fixture
def user_factory(django_command, faker):
    def factory(username=None, email=None, password=None):
        if username is None:
            username = faker.name()
        if email is None:
            email = faker.email()
        if password is None:
            password = faker.password()
        user_id = django_command(
            f"user = User(username='{username}', email='{email}'); user.set_password('{password}'); user.save(); print(user.id)"
        )
        user_id = int(user_id.decode("utf-8"))

        return {
            "username": username,
            "email": email,
            "password": password,
            "id": user_id,
        }

    return factory


@pytest.fixture
def user(user_factory):
    return user_factory()


@pytest.fixture
def record_factory(django_command, user):
    def factory(owner=None, board_size=9):
        if owner is None:
            owner = user
        owner_id = owner["id"]
        record = django_command(
            f"""
owner = User.objects.get(id={owner_id});
record = Record(owner=owner, board_size={board_size});
record.save();
from django.forms import model_to_dict;
d = model_to_dict(record);
# Convert choices to real values
d['winner'] = d['winner'].value;
print(d);
"""
        )
        record = eval(record.decode("utf-8"))
        return record

    return factory


@pytest.fixture
def record(record_factory):
    return record_factory()
