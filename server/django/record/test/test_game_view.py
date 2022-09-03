import pytest

from record.models import Game


@pytest.mark.django_db
def test_create_game(authenticated_client, admin_user, user):
    response = authenticated_client.post('/games/', {'size': 9}, content_type='application/json')
    assert response.status_code == 201
    assert response.data == {
        'id': 1,
        'owner': user.id,
        'size': 9,
    }
    assert Game.objects.filter(id=1).exists()


@pytest.mark.django_db
def test_get_games_empty(authenticated_client):
    response = authenticated_client.get('/games/')
    assert response.status_code == 200
    assert response.data == []


@pytest.mark.django_db
def test_get_games(authenticated_client, game):
    response = authenticated_client.get('/games/')
    assert response.status_code == 200
    assert len(response.data) == 1


@pytest.mark.django_db
def test_get_game(authenticated_client, game):
    response = authenticated_client.get(f'/games/{game.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': game.id,
        'owner': game.owner.id,
        'size': game.size,
        'stones': [],
    }


@pytest.mark.django_db
def test_get_game_with_move(authenticated_client, game):
    game.next_move(0, 0).save()
    response = authenticated_client.get(f'/games/{game.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': game.id,
        'owner': game.owner.id,
        'size': game.size,
        'stones': [{'x': 0, 'y': 0, 'color': 'B'}],
    }


@pytest.mark.django_db
def test_get_game_after_capture(authenticated_client, game):
    # This first black stone will be capture
    game.next_move(0, 0).save()
    game.next_move(0, 1).save()
    game.next_move(8, 8).save()
    game.next_move(1, 0).save()
    response = authenticated_client.get(f'/games/{game.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': game.id,
        'owner': game.owner.id,
        'size': game.size,
        'stones': [
            {'x': 0, 'y': 1, 'color': 'W'},
            {'x': 8, 'y': 8, 'color': 'B'},
            {'x': 1, 'y': 0, 'color': 'W'},
        ],
    }


@pytest.mark.django_db
def test_play_move(authenticated_client, game):
    response = authenticated_client.post(
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
def test_play_two_moves(authenticated_client, game):
    authenticated_client.post(
        f'/games/{game.id}/play/',
        {'x': 0, 'y': 0},
        content_type='application/json',
    )
    response = authenticated_client.post(
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
def test_play_big_capture(authenticated_client, game):
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

    response = authenticated_client.post(
        f'/games/{game.id}/play/',
        {'x': 2, 'y': 1},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {
        'add': [{'x': 2, 'y': 1, 'color': 'W'}],
        'remove': [{'x': 0, 'y': 0}, {'x': 1, 'y': 0}, {'x': 0, 'y': 1}, {'x': 1, 'y': 1}],
    }


@pytest.mark.django_db
def test_delete_game(authenticated_client, game):
    response = authenticated_client.delete(f'/games/{game.id}/')
    assert response.status_code == 204
    assert Game.objects.count() == 0
