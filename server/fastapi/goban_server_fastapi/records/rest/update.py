from datetime import datetime
from typing import Literal

from fastapi import Depends
from pydantic import BaseModel
from sqlalchemy import select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.models import Record
from goban_server_fastapi.rest import app


class RequestModel(BaseModel):
    name: str
    black_player: str
    white_player: str
    comment: str
    handicap: int
    komi: float
    ruleset: Literal["AGA"] | Literal["JPN"] | Literal["CHN"]
    winner: Literal["U"] | Literal["B"] | Literal["W"]


class ResponseModel(BaseModel):
    id: int
    owner: int
    board_size: int
    created: datetime
    name: str
    black_player: str
    white_player: str
    comment: str
    handicap: int
    komi: float
    ruleset: str
    winner: str


@app.put(
    "/api/records/{record_id}/",
    status_code=200,
    response_model=ResponseModel,
)
def update_record(
    record_id: int,
    record_model: RequestModel,
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
):
    with db.session() as session:
        record: Record = session.scalar(
            select(Record)
            .where(Record.id == record_id)
            .where(Record.owner_id == current_user.id)
        )
        for key, value in record_model.dict().items():
            setattr(record, key, value)
        session.commit()
        return {
            **dictify(record),
            "owner": record.owner_id,
        }
