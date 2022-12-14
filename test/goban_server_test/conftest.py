from datetime import datetime
import time
from subprocess import run
from urllib.parse import urljoin

import pytest
import requests
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.auth.models import create_user
from goban_server_fastapi.auth.password import encode_password
from goban_server_fastapi.records.models import Move, Record, next_color
from sqlalchemy import asc, select


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


@pytest.fixture(scope="session", params=["django", "fastapi", "haskell"], autouse=True)
def impl(request):
    return request.param


@pytest.fixture(scope="session", params=["api_factory", "db_factory"], autouse=True)
def factory_method(request):
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
def user_factory(factory_method, client, fastapi_db, faker):
    def api_factory(username=None, email=None, password=None):
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
    """An alternative user factory which uses the FastAPI implementation to interact directly with the database."""
    def db_factory(username=None, email=None, password=None):
        if username is None:
            username = faker.first_name()
        if email is None:
            email = faker.email()
        if password is None:
            password = faker.password()
        password_hash = encode_password(password)
        user = create_user(fastapi_db, username, email, password_hash)
        return {
            "id": user.id,
            "username": username,
            "email": email,
            "password": password,
        }
    if factory_method == "api_factory":
        return api_factory
    elif factory_method == "db_factory":
        return db_factory


@pytest.fixture
def user(user_factory):
    return user_factory()


@pytest.fixture
def record_factory(factory_method, user_client_factory, fastapi_db, user):
    def api_factory(owner=None, board_size=9, **kwargs):
        if owner is None:
            owner = user
        user_client = user_client_factory(owner)
        record = user_client.post(
            "/api/records/",
            json={"board_size": board_size, **kwargs}
        ).json()
        return record

    def db_factory(owner=None, board_size=9, **kwargs):
        if owner is None:
            owner = user
        record = Record(**{
            "owner_id": owner['id'],
            "board_size": board_size,
            "created": datetime.now(),
            "name": "",
            "black_player": "Black",
            "white_player": "White",
            "comment": "",
            "handicap": 0,
            "komi": 7.5,
            "ruleset": "AGA",
            "winner": "U",
            **kwargs,
        })
        with fastapi_db.session() as session:
            session.add(record)
            session.commit()
            return {
                **dictify(record),
                "owner": record.owner_id,
                "created": record.created.isoformat()
            }

    if factory_method == "api_factory":
        return api_factory
    elif factory_method == "db_factory":
        return db_factory


@pytest.fixture
def record(record_factory):
    return record_factory()


@pytest.fixture
def move_factory(factory_method, fastapi_db, user_client, record, user):
    default_record = record

    def api_factory(x, y, record=None):
        if record is None:
            record = default_record
        move = user_client.post(
            f"/api/records/{record['id']}/play/",
            json={"x": x, "y": y}
        ).json()
        return move

    def db_factory(x, y, record=None):
        if record is None:
            record = default_record
        with fastapi_db.session() as session:
            record_model: Record = session.scalar(
                select(Record)
                .where(Record.id == record["id"])
                .where(Record.owner_id == user["id"])
            )
            moves = list(
                session.scalars(
                    select(Move)
                    .where(Move.record_id == record_model.id)
                    .order_by(asc(Move.move))
                )
            )
            color = next_color(record_model, moves)
            position = x + (y * record_model.board_size)
            move_number = len(moves) + 1
            move = Move(
                position=position,
                color=color,
                move=move_number,
                record_id=record_model.id,
            )
            session.add(move)
            session.commit()

    if factory_method == "api_factory":
        return api_factory
    elif factory_method == "db_factory":
        return db_factory


@pytest.fixture
def fastapi_db():
    return DbClient()
