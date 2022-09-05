from django.contrib.auth.models import User
from django.test import Client
import pytest

from record.auth import generate_token
from record.models import Record


@pytest.fixture
def user():
    username = 'John Doe'
    password = 'hunter12'
    user = User(username=username)
    user.set_password(password)
    user.save()
    return user


@pytest.fixture
def authenticated_client(client: Client, user):
    client.defaults['HTTP_AUTHORIZATION'] = f'Bearer {generate_token(user)}'
    return client


@pytest.fixture
def record_factory(user):
    def factory(owner=None, board_size=9):
        if owner is None:
            owner = user
        owner.save()
        record = Record(owner=owner, board_size=board_size)
        record.save()
        return record

    return factory


@pytest.fixture()
def record(record_factory):
    return record_factory()
