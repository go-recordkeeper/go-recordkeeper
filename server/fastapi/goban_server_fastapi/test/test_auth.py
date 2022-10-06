

def test_login(client, user_factory, faker):
    # Generate a user with a known password
    password = faker.password()
    user = user_factory(password=password)

    response = client.post('/api/login/', json={'username': user.username, 'password': password})
    assert response.status_code == 200
    token = response.json()
    assert token

