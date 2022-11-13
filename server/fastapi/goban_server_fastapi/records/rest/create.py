from datetime import datetime
from typing import Literal

from fastapi import Depends
from pydantic import BaseModel

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.models import Record
from goban_server_fastapi.rest import app


class RequestModel(BaseModel):
    board_size: Literal[9] | Literal[13] | Literal[19]
    name: str = ""
    black_player: str = "Black"
    white_player: str = "White"
    comment: str = ""
    handicap: int = 0
    komi: float = 7.5
    ruleset: Literal["AGA"] | Literal["JPN"] | Literal["CHN"] = "AGA"


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


@app.post(
    "/api/records/",
    status_code=201,
    response_model=ResponseModel,
)
def create_record(
    record_model: RequestModel,
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
):
    record = Record(
        owner_id=current_user.id,
        created=datetime.now(),
        winner="U",
        **record_model.dict(),
    )
    with db.session() as session:
        session.add(record)
        session.commit()
        return {
            **dictify(record),
            "owner": record.owner_id,
        }
