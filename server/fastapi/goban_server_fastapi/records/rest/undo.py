from datetime import datetime
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
    "/api/records/{record_id}/undo/",
    status_code=200,
    response_model=ResponseModel,
)
def undo_move(
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
        moves = list(
            session.scalars(
                select(Move).where(Move.record_id == record.id).order_by(asc(Move.move))
            )
        )
        if len(moves) == 0:
            raise HTTPException(status_code=403, detail="No moves to undo")
        move_to_undo = moves[-1]
        undo_x = move_to_undo.position % record.board_size
        undo_y = move_to_undo.position // record.board_size

        board_state = BoardState(size=record.board_size)
        # Play all the moves up to the last move
        for move in moves[:-1]:
            if move.position is not None:
                x = move.position % record.board_size
                y = move.position // record.board_size
                board_state.play_move(x, y, move.color)
        # Simulate playing the last move to determine which stones need to be restored
        captures_to_restore = board_state.play_move(undo_x, undo_y, move_to_undo.color)
        capture_color = next_color(record, moves)

        session.delete(move_to_undo)
        session.commit()

        return {
            "add": [
                {"x": x, "y": y, "color": capture_color}
                for (x, y) in captures_to_restore
            ],
            "remove": [{"x": undo_x, "y": undo_y}],
        }
