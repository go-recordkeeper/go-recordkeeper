from datetime import datetime
from typing import Literal

from pydantic import BaseModel
from sqlalchemy import desc, select

from fastapi import Depends, HTTPException
from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.records.models import Record
from goban_server_fastapi.rest import app


class ListResponseRecord(BaseModel):
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


class ListResponse(BaseModel):
    count: int
    pages: int
    results: list[ListResponseRecord]


@app.get("/api/records/", status_code=200, response_model=ListResponse)
def list_records(
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
    page_size: int = 10,
    page: int = 1,
):
    with db.session() as session:
        count = session.query(Record).filter_by(owner_id=current_user.id).count()
        pages = count // page_size
        # There is always at least an empty page
        if pages == 0:
            pages = 1
        if page_size < 1:
            raise HTTPException(status_code=404, detail="Invalid page_size")
        if page < 1 or page > pages:
            raise HTTPException(status_code=404, detail="Invalid page")
        start = (page - 1) * page_size
        end = start + page_size
        records = session.scalars(
            select(Record)
            .where(Record.owner_id == current_user.id)
            .order_by(desc(Record.created))
            .offset(start)
            .limit(end)
        ).all()
        return {
            "count": count,
            "pages": pages,
            "results": [
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
                for record in records
            ],
        }
