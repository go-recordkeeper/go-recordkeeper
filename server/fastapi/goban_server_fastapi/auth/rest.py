from pydantic import BaseModel, EmailStr
from starlette.responses import JSONResponse, Response

from fastapi import Depends
from goban_server_fastapi.auth.jwt import generate_token, jwt_user
from goban_server_fastapi.auth.models import User, create_user, get_user
from goban_server_fastapi.auth.password import encode_password, verify_password
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.rest import app


class LoginRequest(BaseModel):
    username: str
    password: str


class RegisterRequest(BaseModel):
    username: str
    email: EmailStr
    password: str


class UserResponse(BaseModel):
    id: int
    username: str
    email: str


@app.post(
    "/api/login/",
    status_code=200,
    response_model=str,
    response_description="Fresh authentication token",
    responses={401: {"description": "Authentication failed"}},
)
def login(login: LoginRequest, db: DbClient = Depends()):
    """
    Log a user in using the given username and password.

    If successful, return an authentication token.
    This token should be included in the `Authorization` header of any request that requires authentication, like so:

    `Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6OCwiZXhwIjoxNjY4NTUzNzM3fQ.U_qsYxm88a7-e5laxk83Z6qx3y-mx6dmYXAYtmTCfnw`

    On the docs page, this can be done by clicking "Authorize" in the top right corner and pasting in the token (without quotes).
    """
    u = get_user(db, username=login.username)
    if u is not None:
        if verify_password(login.password, u.password):
            return generate_token(u.id)
    return Response(status_code=401)


@app.post(
    "/api/register/",
    status_code=201,
    response_model=UserResponse,
    response_description="New user created successfully",
    responses={400: {"description": "A user with that username already exists"}},
)
def register(register: RegisterRequest, db: DbClient = Depends()):
    """Register a user with the given username, email, and password."""
    password_hash = encode_password(register.password)
    new_user = create_user(
        db,
        username=register.username,
        email=register.email,
        password_hash=password_hash,
    )
    if new_user is not None:
        return new_user.__dict__
    else:
        return JSONResponse(
            {"username": ["A user with that username already exists."]}, status_code=400
        )


@app.get("/api/user/", status_code=200, response_model=UserResponse)
def user(current_user: User = Depends(jwt_user)):
    """Get the currently logged in user, according to the `Authorization` header."""
    return current_user.__dict__
