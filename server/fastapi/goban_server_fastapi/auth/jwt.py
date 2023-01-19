from datetime import datetime, timedelta, timezone
from typing import Optional

import jwt
from starlette.authentication import AuthenticationError

from fastapi import Depends, Header, HTTPException, Request
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer
from goban_server_fastapi.auth.models import User, get_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.settings import SECRET_KEY


def generate_token(user_id):
    now = datetime.now(tz=timezone.utc)
    return jwt.encode(
        {
            "sub": user_id,
            "iat": now,
            "exp": now + timedelta(days=1),
            "iss": "go-recordkeeper",
            "aud": "go-recordkeeper",
        },
        key=SECRET_KEY,
        algorithm="HS256",
    )


bearer_scheme = HTTPBearer()


def jwt_user(
    db: DbClient = Depends(),
    token: HTTPAuthorizationCredentials = Depends(bearer_scheme),
) -> User:

    try:
        payload = jwt.decode(
            token.credentials,
            key=SECRET_KEY,
            algorithms="HS256",
            audience="go-recordkeeper",
            issuer="go-recordkeeper",
        )
    except jwt.InvalidTokenError:
        raise HTTPException(status_code=403, detail="invalid authorization token")
    if "sub" not in payload:
        raise AuthenticationError(status_code=403, detail="invalid authorization token")

    user = get_user(db, id=payload["sub"])
    if user is None:
        raise AuthenticationError(status_code=403, detail="invalid authorization token")

    return user
