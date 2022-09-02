from django.test import Client
import pytest

from record.models import Game


@pytest.mark.django_db
def test_create_game(client, admin_user):
    response = client.post('/games/', {'size': 9}, content_type='application/json')
    assert response.status_code == 201
    assert response.data == {
        'id': 1,
        'owner': 1,
        'size': 9,
    }
    assert Game.objects.filter(id=1).exists()


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
    assert response.data == {
        'add': [],
        'remove': [],
    }


@pytest.mark.django_db
def test_get_game_with_move(client, game):
    game.next_move(0, 0).save()
    response = client.get(f'/games/{game.id}/')
    assert response.status_code == 200
    assert response.data == {
        'add': [{'x': 0, 'y': 0, 'color': 'B'}],
        'remove': [],
    }


@pytest.mark.django_db
def test_get_game_after_capture(client, game):
    # This first black stone will be capture
    game.next_move(0, 0).save()
    game.next_move(0, 1).save()
    game.next_move(8, 8).save()
    game.next_move(1, 0).save()
    response = client.get(f'/games/{game.id}/')
    assert response.status_code == 200
    assert response.data == {
        'add': [
            {'x': 0, 'y': 1, 'color': 'W'},
            {'x': 8, 'y': 8, 'color': 'B'},
            {'x': 1, 'y': 0, 'color': 'W'},
        ],
        'remove': [],
    }


@pytest.mark.django_db
def test_play_move(client: Client, game):
    response = client.post(
        f'/games/{game.id}/play/',
        {'x': 0, 'y': 0, 'color': 'B'},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {'add': [{'x': 0, 'y': 0, 'color': 'B'}], 'remove': []}
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
        {'x': 0, 'y': 0},
        content_type='application/json',
    )
    response = client.post(
        f'/games/{game.id}/play/',
        {'x': 0, 'y': 1},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {'add': [{'x': 0, 'y': 1, 'color': 'W'}], 'remove': []}
    game.refresh_from_db()
    assert game.moves.count() == 2
    move = game.moves.last()
    assert move.color == 'W'
    assert move.x == 0
    assert move.y == 1
    assert move.move == 2


@pytest.mark.django_db
def test_play_big_capture(client: Client, game):
    # B1 B3 W6
    # B5 B7 W8
    # W2 W4
    game.next_move(0, 0).save()
    game.next_move(0, 2).save()
    game.next_move(1, 0).save()
    game.next_move(1, 2).save()
    game.next_move(0, 1).save()
    game.next_move(2, 0).save()
    game.next_move(1, 1).save()

    response = client.post(
        f'/games/{game.id}/play/',
        {'x': 2, 'y': 1},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {
        'add': [{'x': 2, 'y': 1, 'color': 'W'}],
        'remove': [{'x': 0, 'y': 0}, {'x': 1, 'y': 0}, {'x': 0, 'y': 1}, {'x': 1, 'y': 1}],
    }
