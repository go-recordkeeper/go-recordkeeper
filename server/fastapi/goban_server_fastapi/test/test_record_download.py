from sqlalchemy import select

from goban_server_fastapi.records.models import Move


def test_download_record(db, user_client, record):
    date = record.created.strftime(r"%Y_%m_%d")
    response = user_client.get(f"/api/records/{record.id}/download/")
    assert response.status_code == 200
    assert response.headers["content-type"] == "application/x-go-sgf"
    assert (
        response.headers["content-disposition"]
        == f'attachment; filename="{record.name}_{date}.sgf"'
    )


def test_download_unnamed_record(db, user_client, record_factory):
    record = record_factory(name="", black_player="Mr. Black", white_player="Ms. White")
    date = record.created.strftime(r"%Y_%m_%d")
    response = user_client.get(f"/api/records/{record.id}/download/")
    assert response.status_code == 200
    assert response.headers["content-type"] == "application/x-go-sgf"
    assert (
        response.headers["content-disposition"]
        == f'attachment; filename="Mr._Black_vs_Ms._White_{date}.sgf"'
    )
