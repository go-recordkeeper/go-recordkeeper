from sqlalchemy import delete, select

from typing import List
from fastapi import Depends, HTTPException
from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.records.models import Move, Record
from goban_server_fastapi.rest import app


@app.delete("/api/records/{record_id}/", status_code=204)
def delete_record(
    record_id: int,
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
):
    # raise HTTPException(status_code=555)
    with db.session() as session:
        record: Record = session.scalar(
            select(Record)
            .where(Record.id == record_id)
            .where(Record.owner_id == current_user.id)
        )
        if record is None:
            raise HTTPException(status_code=404)

        session.execute(delete(Move).where(Move.record_id == record_id))
        session.delete(record)
        session.commit()
