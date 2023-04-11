from datetime import datetime
from typing import Literal, Optional

from pydantic import BaseModel
from sqlalchemy import asc, select

from fastapi import Depends, HTTPException
from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.board import BoardState
from goban_server_fastapi.records.models import Move, Record
from goban_server_fastapi.rest import app


class CoordinateModel(BaseModel):
    x: int
    y: int


class Stone(CoordinateModel):
    color: Literal["B", "W"]


class MoveModel(BaseModel):
    position: Optional[int]
    color: Literal["B", "W"]
    captures: list[CoordinateModel]


class GetResponse(BaseModel):
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
    stones: list[Stone]
    moves: list[MoveModel]


@app.get(
    "/api/records/{record_id}/",
    status_code=200,
    response_model=GetResponse,
)
def get_record(
    record_id: int,
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
) -> Optional[Record]:
    with db.session() as session:
        record: Record = session.scalar(
            select(Record)
            .where(Record.id == record_id)
            .where(Record.owner_id == current_user.id)
        )
        if record is None:
            raise HTTPException(status_code=404)
        moves = session.scalars(
            select(Move).where(Move.record_id ==
                               record.id).order_by(asc(Move.move))
        )
        board_state = BoardState(size=record.board_size)
        moves = [
            {
                "position": move.position,
                "color": move.color,
                "captures": [
                    {"x": x, "y": y}
                    for (x, y) in board_state.play_move(
                        move.position % record.board_size,
                        move.position // record.board_size,
                        move.color,
                    )
                ]
                if move.position is not None
                else [],
            }
            for move in moves
        ]
        sorted_stones = sorted(
            board_state.stones.items(), key=lambda x: tuple(reversed(x[0]))
        )
        return {
            **dictify(record),
            "owner": record.owner_id,
            "stones": [
                {"x": x, "y": y, "color": color} for ((x, y), color) in sorted_stones
            ],
            "moves": moves,
        }
