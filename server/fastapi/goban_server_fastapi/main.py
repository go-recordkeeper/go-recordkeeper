from typing import Union

from fastapi import FastAPI
from pydantic import BaseModel

from goban_server_fastapi.auth import PBKDF2PasswordHasher, generate_token
from goban_server_fastapi.models import get_user

class LoginRequest(BaseModel):
    username: str
    password: str

app = FastAPI()


@app.post('/api/login', status_code=200)
def login(login: LoginRequest):
    print('try dat login', login)
    u = get_user(login.username)
    if u is not None:
        print(u, u.username, u.password)
        hasher = PBKDF2PasswordHasher()
        if hasher.verify(login.password, u.password):
            return generate_token(u.id)
    return Response(status_code=401)
