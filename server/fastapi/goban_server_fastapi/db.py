"""
The common SQLAlchemy boilerplate.

There isn't much, but all SQLAlchemy models need a base registry, and it's handy to have a single place to generate the database connection from the settings file.
"""

from contextlib import contextmanager

from sqlalchemy import create_engine
from sqlalchemy.orm import Session, registry

from goban_server_fastapi.settings import *

mapper_registry = registry()
Base = mapper_registry.generate_base()


class DbClient:
    def __init__(self):
        self.engine = create_engine(
            f"postgresql+psycopg2://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}/{POSTGRES_NAME}",
            echo=True,
            future=True,
        )

    @contextmanager
    def session(self):
        with Session(self.engine) as session:
            yield session
