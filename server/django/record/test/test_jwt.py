import jwt
import pytest

from django.conf import settings
from django.contrib.auth.models import User


@pytest.mark.django_db
def test_login(client, user):
    response = client.post(
        '/api/login/',
        {'username': 'John Doe', 'password': 'hunter12'},
        content_type='application/json',
    )
    assert response.status_code == 200
    token = response.json()
    payload = jwt.decode(
        token,
        key=settings.SECRET_KEY,
        algorithms='HS256',
        audience='go-recordkeeper',
        issuer='go-recordkeeper',
    )
    # If no exception, it's a valid token
    assert payload['id'] == user.id


@pytest.mark.django_db
def test_login_wrong_password(client, user):
    response = client.post(
        '/api/login/',
        {'username': 'John Doe', 'password': 'hunter13'},
        content_type='application/json',
    )
    assert response.status_code == 401


@pytest.mark.django_db
def test_get_user(authenticated_client, user):
    response = authenticated_client.get('/api/user/')
    assert response.status_code == 200
    assert response.json() == {
        'id': user.id,
        'username': user.username,
        'email': user.email,
    }


@pytest.mark.django_db
def test_get_user_unauthenticated(client, user):
    response = client.get('/api/user/')
    assert response.status_code == 403


@pytest.mark.django_db
def test_login_with_token(authenticated_client, user):
    response = authenticated_client.post(
        '/api/login/',
        {'username': 'John Doe', 'password': 'hunter12'},
        content_type='application/json',
    )
    assert response.status_code == 200
    token = response.json()
    payload = jwt.decode(
        token,
        key=settings.SECRET_KEY,
        algorithms='HS256',
        audience='go-recordkeeper',
        issuer='go-recordkeeper',
    )
    # If no exception, it's a valid token
    assert payload['id'] == user.id


@pytest.mark.django_db
def test_register(client):
    response = client.post(
        '/api/register/',
        {
            'username': 'jane.doe',
            'email': 'jane.doe@chiquit.ooo',
            'password': 'hunter13',
        },
        content_type='application/json',
    )
    assert response.status_code == 201
    user = User.objects.filter(username='jane.doe', email='jane.doe@chiquit.ooo').get()
    assert response.json() == {
        'id': user.id,
        'username': user.username,
        'email': user.email,
    }
    # Verify the new user can log in
    response = client.post(
        '/api/login/',
        {'username': 'jane.doe', 'password': 'hunter13'},
        content_type='application/json',
    )
    assert response.status_code == 200
