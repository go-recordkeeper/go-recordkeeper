from datetime import datetime
from os import environ
from typing import Optional

from sqlalchemy import Boolean, Column, DateTime, Integer, String, create_engine, insert, select, text
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import Session, registry

from goban_server_fastapi.settings import *

engine = create_engine(f'postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}/{POSTGRES_NAME}', echo=True, future=True)

mapper_registry = registry()
Base = mapper_registry.generate_base()

class User(Base):
    __tablename__ = 'auth_user'

    id = Column(Integer, primary_key=True)
    password = Column(String(128))
    username = Column(String(150))
    email = Column(String(254))
    first_name = Column(String(150))
    last_name = Column(String(150))
    is_superuser = Column(Boolean())
    is_staff = Column(Boolean())
    is_active = Column(Boolean())
    date_joined = Column(DateTime(timezone=True))
    last_login = Column(DateTime(timezone=True))


def get_user(id: Optional[int] = None, username: Optional[str] = None) -> Optional[User]:
    with Session(engine) as session:
        stmt = select(User)
        if id is not None:
            stmt = stmt.where(User.id == id)
        if username is not None:
            stmt = stmt.where(User.username == username)
        return session.scalars(stmt).first()


def create_user(username: str, email: str, password_hash: str) -> Optional[User]:
    user = User(
        username=username,
        email=email,
        password=password_hash,
        first_name='',
        last_name='',
        is_superuser=False,
        is_staff=False,
        is_active=False,
        date_joined=datetime.now(),
        last_login=datetime.now(),
        )
    with Session(engine) as session:
        session.add(user)
        try:
            session.commit()
        except IntegrityError as e:
            return None
    return get_user(username=username)