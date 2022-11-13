def test_undo_move(user_client, record, move_factory):
    move_factory(0, 0, "B")
    response = user_client.post(f"/api/records/{record.id}/undo/")
    assert response.status_code == 200
    assert response.json() == {
        "add": [],
        "remove": [{"x": 0, "y": 0}],
    }


def test_undo_capture(user_client, record, move_factory):
    move_factory(1, 0, "B")
    move_factory(0, 0, "W")
    move_factory(0, 1, "B")
    response = user_client.post(f"/api/records/{record.id}/undo/")
    assert response.status_code == 200
    assert response.json() == {
        "add": [{"x": 0, "y": 0, "color": "W"}],
        "remove": [{"x": 0, "y": 1}],
    }


def test_undo_nothing(user_client, record, move_factory):
    response = user_client.post(f"/api/records/{record.id}/undo/")
    assert response.status_code == 403
