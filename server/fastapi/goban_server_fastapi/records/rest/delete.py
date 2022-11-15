from sqlalchemy import select

from fastapi import Depends, HTTPException
from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.models import Record
from goban_server_fastapi.rest import app


@app.delete("/api/records/{record_id}/", status_code=204)
def delete_record(
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
        session.delete(record)
        session.commit()
