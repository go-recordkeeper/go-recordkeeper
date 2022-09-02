from django.contrib.auth.models import User
from django.test import Client
import pytest

from record.auth import generate_token
from record.models import Game


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
def game_factory(user):
    def factory(owner=None, size=9):
        if owner is None:
            owner = user
        owner.save()
        game = Game(owner=owner, size=size)
        game.save()
        return game

    return factory


@pytest.fixture()
def game(game_factory):
    return game_factory()
