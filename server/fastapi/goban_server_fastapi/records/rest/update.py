from datetime import datetime
from typing import Literal

from pydantic import BaseModel
from sqlalchemy import select

from fastapi import Depends, HTTPException
from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.models import Record
from goban_server_fastapi.rest import app


class UpdateRequest(BaseModel):
    name: str
    black_player: str
    white_player: str
    comment: str
    handicap: int
    komi: float
    ruleset: Literal["AGA", "JPN", "CHN"]
    winner: Literal["U", "B", "W"]


class UpdateResponse(BaseModel):
    id: int
    owner: int
    board_size: Literal[9, 13, 19]
    created: datetime
    name: str
    black_player: str
    white_player: str
    comment: str
    handicap: int
    komi: float
    ruleset: Literal["AGA", "JPN", "CHN"]
    winner: Literal["U", "B", "W"]


@app.put(
    "/api/records/{record_id}/",
    status_code=200,
    response_model=UpdateResponse,
)
def update_record(
    record_id: int,
    record_model: UpdateRequest,
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
        for key, value in record_model.dict().items():
            setattr(record, key, value)
        session.commit()
        return {
            **dictify(record),
            "owner": record.owner_id,
        }
