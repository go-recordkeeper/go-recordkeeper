from datetime import datetime, timedelta, timezone

import jwt
from fastapi import Depends, HTTPException, Request
from starlette.authentication import AuthenticationError

from goban_server_fastapi.auth.models import User, get_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.settings import SECRET_KEY


def generate_token(user_id):
    return jwt.encode(
        {"id": user_id, "exp": datetime.now(tz=timezone.utc) + timedelta(days=1)},
        key=SECRET_KEY,
        algorithm="HS256",
    )


def jwt_user(conn: Request, db: DbClient = Depends()) -> User:
    if "Authorization" not in conn.headers:
        raise HTTPException(status_code=403, detail="invalid authorization token")

    authorization = conn.headers["Authorization"]
    if (not authorization) or (not authorization.startswith("Bearer ")):
        raise HTTPException(status_code=403, detail="invalid authorization token")
    token = authorization.removeprefix("Bearer ")

    try:
        payload = jwt.decode(token, key=SECRET_KEY, algorithms="HS256")
    except jwt.InvalidTokenError:
        raise HTTPException(status_code=403, detail="invalid authorization token")
    if "id" not in payload:
        raise AuthenticationError(status_code=403, detail="invalid authorization token")

    user = get_user(db, id=payload["id"])
    if user is None:
        raise AuthenticationError(status_code=403, detail="invalid authorization token")

    return user
