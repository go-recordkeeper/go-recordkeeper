from typing import Literal

from fastapi import Depends, HTTPException
from pydantic import BaseModel
from sqlalchemy import asc, select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.records.board import BoardState
from goban_server_fastapi.records.models import Move, Record, next_color
from goban_server_fastapi.rest import app


class Point(BaseModel):
    x: int
    y: int


class Stone(Point):
    color: Literal["B"] | Literal["W"]


class ResponseModel(BaseModel):
    add: list[Stone]
    remove: list[Point]


@app.post(
    "/api/records/{record_id}/play/",
    status_code=200,
    response_model=ResponseModel,
)
def play_move(
    record_id: int,
    point: Point,
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
        moves = list(
            session.scalars(
                select(Move).where(Move.record_id == record.id).order_by(asc(Move.move))
            )
        )
        board_state = BoardState(size=record.board_size)
        for move in moves:
            x = move.position % record.board_size
            y = move.position // record.board_size
            board_state.play_move(x, y, move.color)

        color = next_color(record, moves)
        captures = board_state.play_move(point.x, point.y, color)
        position = point.x + (point.y * record.board_size)
        move_number = len(moves) + 1

        move = Move(
            position=position,
            color=color,
            move=move_number,
            record_id=record.id,
        )
        session.add(move)
        session.commit()

        return {
            "add": [{"x": point.x, "y": point.y, "color": color}],
            "remove": [{"x": x, "y": y} for (x, y) in captures],
        }
