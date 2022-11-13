from datetime import datetime

from fastapi import Depends
from pydantic import BaseModel
from sqlalchemy import desc, select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.records.db import Record
from goban_server_fastapi.rest import app


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


@app.get("/api/records/", status_code=200, response_model=list[ResponseModel])
def list_records(db: DbClient = Depends(), current_user: User = Depends(jwt_user)):
    with db.session() as session:
        return [
            {
                "id": record.id,
                "owner": record.owner_id,
                "board_size": record.board_size,
                "created": record.created,
                "name": record.name,
                "black_player": record.black_player,
                "white_player": record.white_player,
                "comment": record.comment,
                "handicap": record.handicap,
                "komi": record.komi,
                "ruleset": record.ruleset,
                "winner": record.winner,
            }
            for record in session.scalars(
                select(Record)
                .where(Record.owner_id == current_user.id)
                .order_by(desc(Record.created))
            )
        ]
