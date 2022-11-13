from fastapi import Depends, HTTPException
from sqlalchemy import asc, select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.records.models import Move, Record, next_color
from goban_server_fastapi.rest import app


@app.post("/api/records/{record_id}/pass/", status_code=201)
def pass_turn(
    record_id: int,
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
):
    with db.session() as session:
        record: Record = session.scalar(
            select(Record)
            .where(Record.id == record_id)
            .where(Record.owner_id == current_user.id)
        )
        if record is None:
            raise HTTPException(status_code=404)
        moves = list(
            session.scalars(
                select(Move).where(Move.record_id == record.id).order_by(asc(Move.move))
            )
        )
        color = next_color(record, moves)
        move_number = len(moves) + 1

        move = Move(
            position=None,
            color=color,
            move=move_number,
            record_id=record.id,
        )
        session.add(move)
        session.commit()
