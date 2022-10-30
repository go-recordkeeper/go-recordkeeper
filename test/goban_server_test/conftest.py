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
