from os import environ
from typing import Optional

from sqlalchemy import Column, Integer, String, create_engine, select, text
from sqlalchemy.orm import Session, registry

from goban_server_fastapi.settings import *

engine = create_engine(f'postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}/{POSTGRES_NAME}', echo=True, future=True)

# with Session(engine) as session:
#     result = session.execute(text('SELECT * FROM pg_catalog.pg_tables;'))
#     for row in result.all():
#         print(row)

mapper_registry = registry()
Base = mapper_registry.generate_base()

class User(Base):
    __tablename__ = 'auth_user'

    id = Column(Integer, primary_key=True)
    password = Column(String(128))
    username = Column(String(150))
    email = Column(String(254))

def get_user(username: str) -> Optional[User]:
    with Session(engine) as session:
        stmt = select(User).where(User.username == username)
        return session.scalars(stmt).first()