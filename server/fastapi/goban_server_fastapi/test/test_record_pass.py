from sqlalchemy import select

from goban_server_fastapi.records.models import Move


def test_pass_turn(db, user_client, record):
    response = user_client.post(f"/api/records/{record.id}/pass/")
    assert response.status_code == 201
    with db.session() as session:
        moves = list(session.scalars(select(Move).where(Move.record_id == record.id)))
        assert len(moves) == 1
        move = moves[0]
        assert move.position == None
        assert move.move == 1
