from datetime import datetime
from typing import Optional

from sqlalchemy import Boolean, Column, DateTime, Integer, String, select
from sqlalchemy.exc import IntegrityError

from goban_server_fastapi.db import Base, DbClient


class User(Base):
    __tablename__ = "auth_user"

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


def get_user(
    db: DbClient,
    id: Optional[int] = None,
    username: Optional[str] = None,
) -> Optional[User]:
    with db.session() as session:
        stmt = select(User)
        if id is not None:
            stmt = stmt.where(User.id == id)
        if username is not None:
            stmt = stmt.where(User.username == username)
        return session.scalars(stmt).first()


def create_user(
    db: DbClient,
    username: str,
    email: str,
    password_hash: str,
) -> Optional[User]:
    user = User(
        username=username,
        email=email,
        password=password_hash,
        first_name="",
        last_name="",
        is_superuser=False,
        is_staff=False,
        is_active=False,
        date_joined=datetime.now(),
        last_login=datetime.now(),
    )
    with db.session() as session:
        session.add(user)
        try:
            session.commit()
        except IntegrityError as e:
            return None
        # Fetch the id of the newly created entity
        user.id
        return user
