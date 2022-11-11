from datetime import datetime
from typing import List

from fastapi import Depends
from pydantic import BaseModel
from sqlalchemy import Column, DateTime, Float, Integer, String, desc, select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import Base, DbClient
from goban_server_fastapi.rest import app


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


class RecordModel(BaseModel):
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


def record_to_dict(record) -> dict:
    return {
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


@app.get("/api/records/", status_code=200, response_model=List[RecordModel])
def records(db: DbClient = Depends(), current_user: User = Depends(jwt_user)):
    return [record_to_dict(record) for record in get_records(db, current_user.id)]
