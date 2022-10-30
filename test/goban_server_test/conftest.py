from subprocess import call
from urllib.parse import urljoin

import pytest
import requests


@pytest.fixture(scope="session", autouse=True)
def init_db():
    call(["docker", "compose", "down"])
    call(
        ["docker", "compose", "run", "--rm", "django", "python", "manage.py", "migrate"]
    )
    yield
    # call(["docker", "compose", "run", "--rm", "django", "python", "manage.py", "migrate", "goban", "zero"])
    call(["docker", "compose", "stop"])


@pytest.fixture(scope="function", autouse=True)
def clean_db():
    call(
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
    yield


@pytest.fixture(scope="session", params=["django", "fastapi"])
def impl(request):
    return request.param


@pytest.fixture(scope="session")
def server_under_test(impl):
    call(["docker", "compose", "up", "-d", "--wait", impl])
    yield
    call(["docker", "compose", "stop", impl])


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
