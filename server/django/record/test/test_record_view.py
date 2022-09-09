from anys import ANY_DATETIME_STR, ANY_INT
import pytest

from record.models import Record


@pytest.mark.django_db
# The admin user fixture is ensuring there is another user in the DB
def test_create_sparse_record(authenticated_client, admin_user, user):
    response = authenticated_client.post(
        '/records/', {'board_size': 9}, content_type='application/json'
    )
    assert response.status_code == 201
    assert response.data == {
        'id': ANY_INT,
        'owner': user.id,
        'board_size': 9,
        'created': ANY_DATETIME_STR,
        'name': '',
        'black_player': 'Black',
        'white_player': 'White',
        'comment': '',
        'handicap': 0,
        'komi': 7.5,
        'ruleset': 'AGA',
    }
    assert Record.objects.filter(id=response.data['id']).exists()


@pytest.mark.django_db
def test_create_record(authenticated_client, user):
    response = authenticated_client.post(
        '/records/',
        {
            'board_size': 19,
            'name': 'Kickass Game',
            'black_player': 'Preto',
            'white_player': 'Branco',
            'comment': 'tee hee',
            'handicap': 6,
            'komi': 0,
            'ruleset': 'JPN',
        },
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {
        'id': ANY_INT,
        'owner': user.id,
        'board_size': 19,
        'created': ANY_DATETIME_STR,
        'name': 'Kickass Game',
        'black_player': 'Preto',
        'white_player': 'Branco',
        'comment': 'tee hee',
        'handicap': 6,
        'komi': 0.0,
        'ruleset': 'JPN',
    }
    assert Record.objects.filter(id=response.data['id']).exists()


@pytest.mark.django_db
def test_update_record(authenticated_client, user, record):
    response = authenticated_client.put(
        f'/records/{record.id}/',
        {
            'name': 'Kickass Game',
            'black_player': 'Preto',
            'white_player': 'Branco',
            'comment': 'tee hee',
            'handicap': 6,
            'komi': 0,
            'ruleset': 'JPN',
        },
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {
        'id': ANY_INT,
        'owner': user.id,
        'board_size': 9,
        'created': ANY_DATETIME_STR,
        'name': 'Kickass Game',
        'black_player': 'Preto',
        'white_player': 'Branco',
        'comment': 'tee hee',
        'handicap': 6,
        'komi': 0.0,
        'ruleset': 'JPN',
    }
    record.refresh_from_db()
    assert record.name == response.data['name']
    assert record.black_player == response.data['black_player']
    assert record.white_player == response.data['white_player']
    assert record.comment == response.data['comment']
    assert record.handicap == response.data['handicap']
    assert record.komi == response.data['komi']
    assert record.ruleset == response.data['ruleset']


@pytest.mark.django_db
def test_get_records_empty(authenticated_client):
    response = authenticated_client.get('/records/')
    assert response.status_code == 200
    assert response.data == []


@pytest.mark.django_db
def test_get_records(authenticated_client, record):
    response = authenticated_client.get('/records/')
    assert response.status_code == 200
    assert len(response.data) == 1


@pytest.mark.django_db
def test_get_record(authenticated_client, record):
    response = authenticated_client.get(f'/records/{record.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': record.id,
        'owner': record.owner.id,
        'board_size': record.board_size,
        'created': ANY_DATETIME_STR,
        'name': record.name,
        'black_player': record.black_player,
        'white_player': record.white_player,
        'comment': record.comment,
        'handicap': record.handicap,
        'komi': record.komi,
        'ruleset': record.ruleset,
        'stones': [],
    }


@pytest.mark.django_db
def test_get_record_with_move(authenticated_client, record):
    record.next_move(0, 0).save()
    response = authenticated_client.get(f'/records/{record.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': record.id,
        'owner': record.owner.id,
        'board_size': record.board_size,
        'created': ANY_DATETIME_STR,
        'name': record.name,
        'black_player': record.black_player,
        'white_player': record.white_player,
        'comment': record.comment,
        'handicap': record.handicap,
        'komi': record.komi,
        'ruleset': record.ruleset,
        'stones': [{'x': 0, 'y': 0, 'color': 'B'}],
    }


@pytest.mark.django_db
def test_get_record_with_pass(authenticated_client, record):
    record.pass_turn().save()
    response = authenticated_client.get(f'/records/{record.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': record.id,
        'owner': record.owner.id,
        'board_size': record.board_size,
        'created': ANY_DATETIME_STR,
        'name': record.name,
        'black_player': record.black_player,
        'white_player': record.white_player,
        'comment': record.comment,
        'handicap': record.handicap,
        'komi': record.komi,
        'ruleset': record.ruleset,
        'stones': [],
    }


@pytest.mark.django_db
def test_get_record_after_capture(authenticated_client, record):
    # This first black stone will be capture
    record.next_move(0, 0).save()
    record.next_move(0, 1).save()
    record.pass_turn().save()
    record.next_move(1, 0).save()
    response = authenticated_client.get(f'/records/{record.id}/')
    assert response.status_code == 200
    assert response.data == {
        'id': record.id,
        'owner': record.owner.id,
        'board_size': record.board_size,
        'created': ANY_DATETIME_STR,
        'name': record.name,
        'black_player': record.black_player,
        'white_player': record.white_player,
        'comment': record.comment,
        'handicap': record.handicap,
        'komi': record.komi,
        'ruleset': record.ruleset,
        'stones': [
            {'x': 0, 'y': 1, 'color': 'W'},
            {'x': 1, 'y': 0, 'color': 'W'},
        ],
    }


@pytest.mark.django_db
def test_play_move(authenticated_client, record):
    response = authenticated_client.post(
        f'/records/{record.id}/play/',
        {'x': 0, 'y': 0, 'color': 'B'},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {'add': [{'x': 0, 'y': 0, 'color': 'B'}], 'remove': []}
    record.refresh_from_db()
    assert record.moves.count() == 1
    move = record.moves.first()
    assert move.color == 'B'
    assert move.x == 0
    assert move.y == 0
    assert move.move == 1


@pytest.mark.django_db
def test_play_two_moves(authenticated_client, record):
    authenticated_client.post(
        f'/records/{record.id}/play/',
        {'x': 0, 'y': 0},
        content_type='application/json',
    )
    response = authenticated_client.post(
        f'/records/{record.id}/play/',
        {'x': 0, 'y': 1},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {'add': [{'x': 0, 'y': 1, 'color': 'W'}], 'remove': []}
    record.refresh_from_db()
    assert record.moves.count() == 2
    move = record.moves.last()
    assert move.color == 'W'
    assert move.x == 0
    assert move.y == 1
    assert move.move == 2


@pytest.mark.django_db
def test_play_after_pass(authenticated_client, record):
    record.pass_turn().save()
    response = authenticated_client.post(
        f'/records/{record.id}/play/',
        {'x': 0, 'y': 0},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {'add': [{'x': 0, 'y': 0, 'color': 'W'}], 'remove': []}
    record.refresh_from_db()
    assert record.moves.count() == 2
    move = record.moves.last()
    assert move.color == 'W'
    assert move.x == 0
    assert move.y == 0
    assert move.move == 2


@pytest.mark.django_db
def test_play_big_capture(authenticated_client, record):
    # B1 B3 W6
    # B5 B7 W8
    # W2 W4
    record.next_move(0, 0).save()
    record.next_move(0, 2).save()
    record.next_move(1, 0).save()
    record.next_move(1, 2).save()
    record.next_move(0, 1).save()
    record.next_move(2, 0).save()
    record.next_move(1, 1).save()

    response = authenticated_client.post(
        f'/records/{record.id}/play/',
        {'x': 2, 'y': 1},
        content_type='application/json',
    )
    assert response.status_code == 201
    assert response.data == {
        'add': [{'x': 2, 'y': 1, 'color': 'W'}],
        'remove': [{'x': 0, 'y': 0}, {'x': 1, 'y': 0}, {'x': 0, 'y': 1}, {'x': 1, 'y': 1}],
    }


@pytest.mark.django_db
def test_delete_record(authenticated_client, record):
    response = authenticated_client.delete(f'/records/{record.id}/')
    assert response.status_code == 204
    assert Record.objects.count() == 0


@pytest.mark.django_db
def test_undo(authenticated_client, record):
    record.next_move(0, 0).save()
    response = authenticated_client.post(
        f'/records/{record.id}/undo/',
    )
    assert response.data == {'add': [], 'remove': [{'x': 0, 'y': 0}]}
    record.refresh_from_db()
    assert not record.moves.exists()


@pytest.mark.django_db
def test_undo_capture(authenticated_client, record):
    record.next_move(0, 0).save()
    record.next_move(0, 1).save()
    record.pass_turn().save()
    # This move captures the 1-1 stone
    record.next_move(1, 0).save()
    response = authenticated_client.post(
        f'/records/{record.id}/undo/',
    )
    assert response.data == {'add': [{'x': 0, 'y': 0, 'color': 'B'}], 'remove': [{'x': 1, 'y': 0}]}
    record.refresh_from_db()
    assert record.moves.count() == 3


@pytest.mark.django_db
def test_undo_pass(authenticated_client, record):
    record.pass_turn().save()
    response = authenticated_client.post(
        f'/records/{record.id}/undo/',
    )
    assert response.data == {'add': [], 'remove': []}
    record.refresh_from_db()
    assert record.moves.count() == 0


@pytest.mark.django_db
def test_pass(authenticated_client, record):
    response = authenticated_client.post(
        f'/records/{record.id}/pass/',
    )
    assert response.data is None
    record.refresh_from_db()
    assert record.moves.count() == 1
    move = record.moves.first()
    assert move.color == 'B'
    assert move.position is None
    assert move.move == 1
