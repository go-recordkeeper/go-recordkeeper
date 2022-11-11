from fastapi import Depends
from pydantic import BaseModel, EmailStr
from starlette.responses import JSONResponse, Response

from goban_server_fastapi.auth.jwt import generate_token, jwt_user
from goban_server_fastapi.auth.models import User, create_user, get_user
from goban_server_fastapi.auth.password import PBKDF2PasswordHasher
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


@app.post("/api/login/", status_code=200)
def login(login: LoginRequest, db: DbClient = Depends()):
    u = get_user(db, username=login.username)
    if u is not None:
        hasher = PBKDF2PasswordHasher()
        if hasher.verify(login.password, u.password):
            return generate_token(u.id)
    return Response(status_code=401)


@app.post("/api/register/", status_code=201, response_model=UserResponse)
def register(register: RegisterRequest, db: DbClient = Depends()):
    hasher = PBKDF2PasswordHasher()
    password_hash = hasher.encode(register.password, hasher.salt())
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
    return current_user.__dict__
