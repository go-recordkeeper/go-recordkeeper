import pytest


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
