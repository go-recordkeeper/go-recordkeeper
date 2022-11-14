from datetime import datetime, timedelta, timezone
from typing import Optional

import jwt
from fastapi import Depends, Header, HTTPException, Request
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
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


bearer_scheme = HTTPBearer()


def jwt_user(
    db: DbClient = Depends(),
    token: HTTPAuthorizationCredentials = Depends(bearer_scheme),
) -> User:

    try:
        payload = jwt.decode(token.credentials, key=SECRET_KEY, algorithms="HS256")
    except jwt.InvalidTokenError:
        raise HTTPException(status_code=403, detail="invalid authorization token")
    if "id" not in payload:
        raise AuthenticationError(status_code=403, detail="invalid authorization token")

    user = get_user(db, id=payload["id"])
    if user is None:
        raise AuthenticationError(status_code=403, detail="invalid authorization token")

    return user
