from django.conf import settings
import jwt
import pytest


@pytest.mark.django_db
def test_login(client, user):
    response = client.post(
        '/login/', {'username': 'John Doe', 'password': 'hunter12'}, content_type='application/json'
    )
    assert response.status_code == 200
    token = response.json()
    payload = jwt.decode(token, key=settings.SECRET_KEY, algorithms='HS256')
    # If no exception, it's a valid token
    assert payload['id'] == user.id


@pytest.mark.django_db
def test_get_user(authenticated_client, user):
    response = authenticated_client.get('/user/')
    assert response.status_code == 200
    assert response.json() == {
        'id': user.id,
        'username': user.username,
        'email': user.email,
    }


@pytest.mark.django_db
def test_get_user_unauthenticated(client, user):
    response = client.get('/user/')
    assert response.status_code == 403


@pytest.mark.django_db
def test_login_with_token(authenticated_client, user):
    response = authenticated_client.post(
        '/login/',
        {'username': 'John Doe', 'password': 'hunter12'},
        content_type='application/json',
    )
    assert response.status_code == 200
    token = response.json()
    payload = jwt.decode(token, key=settings.SECRET_KEY, algorithms='HS256')
    # If no exception, it's a valid token
    assert payload['id'] == user.id
