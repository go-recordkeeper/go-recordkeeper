from anys import ANY_DATETIME_STR


def test_delete_record(user_client, record):
    response = user_client.delete(f"/api/records/{record.id}/")
    assert response.status_code == 204
