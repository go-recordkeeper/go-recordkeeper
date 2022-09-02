from django.contrib.auth.models import User
import pytest

from record.models import Game


@pytest.fixture()
def game_factory():
    def factory(owner=None, size=9):
        if owner is None:
            owner = User(email='test@chiquit.ooo', username='Ms. Test')
        owner.save()
        game = Game(owner=owner, size=size)
        game.save()
        return game

    return factory


@pytest.fixture()
def game(game_factory):
    return game_factory()
