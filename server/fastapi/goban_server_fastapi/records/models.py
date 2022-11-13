from pydantic import BaseModel
from sqlalchemy import Column, DateTime, Float, Integer, String, desc, select

from goban_server_fastapi.db import Base


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


def next_color(record: Record, moves: list[Move]):
    if len(moves) <= record.handicap:
        return "B"
    if moves[-1].color == "W":
        return "B"
    return "W"
