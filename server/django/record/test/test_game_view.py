from django.test import Client
import pytest

from record.models import Move


@pytest.mark.django_db
def test_get_games_empty(client):
    response = client.get('/games/')
    assert response.status_code == 200
    assert response.data == []


@pytest.mark.django_db
def test_get_games(client, game):
    response = client.get('/games/')
    assert response.status_code == 200
    assert len(response.data) == 1


@pytest.mark.django_db
def test_get_game(client, game):
    response = client.get(f'/games/{game.id}/')
    assert response.status_code == 200
    assert response.data is not None


@pytest.mark.django_db
def test_play_move(client: Client, game):
    response = client.post(
        f'/games/{game.id}/play/',
        {'x': 0, 'y': 0, 'color': 'B'},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == []
    game.refresh_from_db()
    assert game.moves.count() == 1
    move = game.moves.first()
    assert move.color == 'B'
    assert move.x == 0
    assert move.y == 0
    assert move.move == 1


@pytest.mark.django_db
def test_play_two_moves(client: Client, game):
    client.post(
        f'/games/{game.id}/play/',
        {'x': 0, 'y': 0, 'color': 'B'},
        content_type='application/json',
    )
    response = client.post(
        f'/games/{game.id}/play/',
        {'x': 0, 'y': 1, 'color': 'W'},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == []
    game.refresh_from_db()
    assert game.moves.count() == 2
    move = game.moves.last()
    assert move.color == 'W'
    assert move.x == 0
    assert move.y == 1
    assert move.move == 2
