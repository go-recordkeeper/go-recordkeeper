from fastapi.testclient import TestClient
import pytest
from sqlalchemy import delete
from sqlalchemy.orm import Session

from goban_server_fastapi.auth import PBKDF2PasswordHasher
from goban_server_fastapi.main import app
from goban_server_fastapi.models import DbClient, Move, Record, User


@pytest.fixture
def client():
    return TestClient(app)


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
