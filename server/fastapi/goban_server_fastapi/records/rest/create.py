from datetime import datetime
from typing import Literal

from pydantic import BaseModel

from fastapi import Depends
from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.models import Record
from goban_server_fastapi.rest import app


class CreateRequest(BaseModel):
    board_size: Literal[9, 13, 19]
    name: str = ""
    black_player: str = "Black"
    white_player: str = "White"
    comment: str = ""
    handicap: int = 0
    komi: float = 7.5
    ruleset: Literal["AGA", "JPN", "CHN"] = "AGA"


class CreateResponse(BaseModel):
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


@app.post(
    "/api/records/",
    status_code=201,
    response_model=CreateResponse,
)
def create_record(
    record_model: CreateRequest,
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
