from datetime import datetime
from typing import Literal, Optional

from fastapi import Depends, Response
from pydantic import BaseModel
from sqlalchemy import select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient, dictify
from goban_server_fastapi.records.board import BoardState
from goban_server_fastapi.records.db import Move, Record
from goban_server_fastapi.rest import app


class CoordinateModel(BaseModel):
    x: int
    y: int


class StoneModel(CoordinateModel):
    color: Literal["B"] | Literal["W"]


class MoveModel(BaseModel):
    position: int
    color: Literal["B"] | Literal["W"]
    captures: list[CoordinateModel]


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
    stones: list[StoneModel]
    moves: list[MoveModel]


@app.get(
    "/api/records/{record_id}/",
    status_code=200,
    response_model=ResponseModel,
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
            return Response(status_code=404)
        moves = session.scalars(select(Move).where(Move.record_id == record.id))
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
                ],
            }
            for move in moves
        ]

        return {
            **dictify(record),
            "owner": record.owner_id,
            "stones": [
                {"x": x, "y": y, "color": color}
                for ((x, y), color) in board_state.stones.items()
            ],
            "moves": moves,
        }
