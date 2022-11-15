from datetime import datetime
from typing import Literal, Optional

import pytest
from sqlalchemy import delete

from fastapi.testclient import TestClient
from goban_server_fastapi.auth.jwt import generate_token
from goban_server_fastapi.auth.models import User, create_user
from goban_server_fastapi.auth.password import encode_password
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.main import app
from goban_server_fastapi.records.models import Move, Record


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
    with client.session() as session:
        session.execute(delete(Move))
        session.execute(delete(Record))
        session.execute(delete(User))
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
        password_hash = encode_password(password)
        return create_user(db, username, email, password_hash)

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
        with db.session() as session:
            session.add(record)
            session.commit()
            # Access the ID so that it's cached for use outside the session
            record.id
        return record

    return factory


@pytest.fixture
def record(record_factory):
    return record_factory()


@pytest.fixture
def move_factory(db: DbClient, record, faker):
    default_record = record

    def factory(
        x: int,
        y: int,
        color: Literal["B", "W"],
        record: Optional[Record] = None,
    ):
        if record is None:
            record = default_record
        position = x + (y * record.board_size)
        with db.session() as session:
            move_number = (
                session.query(Move).where(Move.record_id == record.id).count() + 1
            )
            move = Move(
                **{
                    "position": position,
                    "color": color,
                    "move": move_number,
                    "record_id": record.id,
                }
            )
            session.add(move)
            session.commit()
            # Access the ID so that it's cached for use outside the session
            move.id
        return move

    return factory
