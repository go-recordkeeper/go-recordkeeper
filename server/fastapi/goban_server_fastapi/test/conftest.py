from datetime import datetime

import pytest
from fastapi.testclient import TestClient
from sqlalchemy import delete
from sqlalchemy.orm import Session

from goban_server_fastapi.auth import PBKDF2PasswordHasher, generate_token
from goban_server_fastapi.main import app
from goban_server_fastapi.models import DbClient, Move, Record, User


@pytest.fixture
def client():
    test_client = TestClient(app)
    return test_client


@pytest.fixture
def user_client_factory():
    def factory(user):
        test_client = TestClient(app)
        token = generate_token(user.id)
        test_client.headers["Authorization"] = f"Bearer {token}"
        return test_client

    return factory


@pytest.fixture
def user_client(user_client_factory, user):
    return user_client_factory(user)


def clean_db(client: DbClient):
    """Delete all relevant data from the database"""
    with Session(client.engine) as session:
        session.execute(delete(User))
        session.execute(delete(Record))
        session.execute(delete(Move))
        session.commit()


@pytest.fixture(autouse=True)
def db():
    client = DbClient()
    clean_db(client)
    yield client
    # clean_db(client)


@pytest.fixture
def user_factory(db: DbClient, faker):
    def factory(username=None, email=None, password=None):
        if username is None:
            username = faker.name()
        if email is None:
            email = faker.email()
        if password is None:
            password = faker.password()
        hasher = PBKDF2PasswordHasher()
        password_hash = hasher.encode(password, hasher.salt())
        return db.create_user(username, email, password_hash)

    return factory


@pytest.fixture
def user(user_factory):
    return user_factory()


@pytest.fixture
def record_factory(db: DbClient, user, faker):
    def factory(**kwargs):
        record = Record(
            **{
                "owner_id": user.id,
                "board_size": 19,
                "created": datetime.now(),
                "name": faker.sentence(),
                "black_player": "Black",
                "white_player": "White",
                "comment": "",
                "handicap": 0,
                "komi": 7.5,
                "ruleset": "AGA",
                "winner": "U",
                **kwargs,
            }
        )
        db.create_record(record)
        return record

    return factory


@pytest.fixture
def record(record_factory):
    return record_factory()
