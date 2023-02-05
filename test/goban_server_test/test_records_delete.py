from anys import ANY_DATETIME_STR


def test_delete_record(user_client, move_factory, record):
    move_factory(0, 0)
    move_factory(1, 0)
    response = user_client.delete(
        f"/api/records/{record['id']}/",
    )
    assert response.status_code == 204

    # Verify changes were persisted to the database
    response = user_client.get(f"/api/records/{record['id']}/")
    assert response.status_code == 404

    # Verify that deleting a record that does not exist fails
    response = user_client.delete(
        "/api/records/9999/",
    )
    assert response.status_code == 404
