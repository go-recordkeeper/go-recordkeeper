import pytest

def test_login(client, user_factory, faker):
    # Generate a user with a known password
    password = faker.password()
    user = user_factory(password=password)

    response = client.post('/api/login/', json={'username': user.username, 'password': password})
    assert response.status_code == 200
    token = response.json()
    assert token


def test_login_wrong_password(client, user_factory, faker):
    # Generate a user with a known password
    password = faker.password()
    user = user_factory(password=password)

    response = client.post('/api/login/', json={'username': user.username, 'password': password + 'butnotactually'})
    assert response.status_code == 401


def test_register(db, client, faker):
    username = faker.name()
    email = faker.email()
    password = faker.password()
    response = client.post('/api/register/', json={'username': username, 'email': email, 'password': password})
    assert response.status_code == 201
    user = response.json()
    assert user['username'] == username
    assert user['email'] == email
    assert db.get_user(id=user['id'])


@pytest.mark.parametrize(
    'json',
    [
        {},
        {'username': 'User'},
        {'email': 'email@go.chiquit.ooo'},
        {'password': 'p4ssw0rd'},
        {'username': 'User', 'email': 'email@go.chiquit.ooo'},
        {'username': 'User', 'password': 'p4ssw0rd'},
        {'email': 'email@go.chiquit.ooo', 'password': 'p4ssw0rd'},
        {'username': None, 'email': None, 'password': None},
        {'username': '', 'email': '', 'password': ''},
        {'username': 'User', 'email': 'notanemail', 'password': 'p4ssw0rd'},
    ],
    ids=[
        'empty',
        'only-username',
        'only-email',
        'only-password',
        'no-password',
        'no-email',
        'no-username',
        'nones',
        'empty-strings',
        'bad-email',
    ]
)
def test_register_invalid_input(client, json):
    response = client.post('/api/register/', json=json)
    assert response.status_code == 400


def test_register_duplicate_username(client, user, faker):
    username = user.username
    email = faker.email()
    password = faker.password()
    response = client.post('/api/register/', json={'username': username, 'email': email, 'password': password})
    assert response.status_code == 400


def test_get_user(user_client, user):
    response = user_client.get('/api/user/')
    assert response.status_code == 200
    user_dict = response.json()
    assert user_dict['id'] == user.id
    assert user_dict['username'] == user.username
    assert user_dict['email'] == user.email


def test_get_user_no_token(client, user):
    response = client.get('/api/user/')
    assert response.status_code == 401


def test_full_auth_flow(client, user_client_factory, faker):
    # Register a user
    username = faker.name()
    email = faker.email()
    password = faker.password()
    response = client.post('/api/register/', json={'username': username, 'email': email, 'password': password})
    assert response.status_code == 201
    register_user_dict = response.json()

    # Log the user in
    response = client.post('/api/login/', json={'username': username, 'password': password})
    assert response.status_code == 200
    token = response.json()
    client.headers['Authorization'] = f'Bearer {token}'

    # Get the logged in user
    response = client.get('/api/user/')
    assert response.status_code == 200
    assert response.json() == register_user_dict