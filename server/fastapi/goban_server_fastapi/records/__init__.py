from datetime import datetime
from typing import List, Literal, Optional

from fastapi import Depends, Response
from pydantic import BaseModel
from sqlalchemy import Column, DateTime, Float, Integer, String, desc, select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import Base, DbClient, dictify
from goban_server_fastapi.rest import app
from goban_server_fastapi.records.board import BoardState


class Record(Base):
    __tablename__ = "record_record"

    id = Column(Integer, primary_key=True)
    board_size = Column(Integer)
    created = Column(DateTime(timezone=True))
    name = Column(String(200))
    black_player = Column(String(200))
    white_player = Column(String(200))
    comment = Column(String(400))
    handicap = Column(Integer)
    komi = Column(Float)
    ruleset = Column(String(3))
    owner_id = Column(Integer)
    winner = Column(String(1))


class Move(Base):
    __tablename__ = "record_move"

    id = Column(Integer, primary_key=True)
    position = Column(Integer)
    color = Column(String(1))
    move = Column(Integer)
    record_id = Column(Integer)


def create_record(db: DbClient, record: Record):
    with db.session() as session:
        session.add(record)
        session.commit()
        # Access the id now to cache it while the session is still available
        record.id


def get_records(db: DbClient, user_id) -> list[Record]:
    with db.session() as session:
        stmt = (
            select(Record)
            .where(Record.owner_id == user_id)
            .order_by(desc(Record.created))
        )
        # Use list to resolve the query so results are available outside the session
        return list(session.scalars(stmt))


class ListRecordModel(BaseModel):
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


@app.get("/api/records/", status_code=200, response_model=List[ListRecordModel])
def list_records(db: DbClient = Depends(), current_user: User = Depends(jwt_user)):
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
        for record in get_records(db, current_user.id)
    ]


class CoordinateModel(BaseModel):
    x: int
    y: int


class StoneModel(CoordinateModel):
    color: Literal["B"] | Literal["W"]


class MoveModel(BaseModel):
    position: int
    color: Literal["B"] | Literal["W"]
    captures: list[CoordinateModel]


class GetRecordModel(ListRecordModel):
    stones: list[StoneModel]
    moves: list[MoveModel]


@app.get(
    "/api/records/{record_id}/",
    status_code=200,
    response_model=GetRecordModel,
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


class CreateRecordRequestModel(BaseModel):
    board_size: Literal[9] | Literal[13] | Literal[19]
    name: str = ""
    black_player: str = "Black"
    white_player: str = "White"
    comment: str = ""
    handicap: int = 0
    komi: float = 7.5
    ruleset: Literal["AGA"] | Literal["JPN"] | Literal["CHN"] = "AGA"


class CreateRecordResponseModel(BaseModel):
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
    response_model=CreateRecordResponseModel,
)
def _create_record(
    record_model: CreateRecordRequestModel,
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
