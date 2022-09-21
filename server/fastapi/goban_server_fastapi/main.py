from typing import Union

from fastapi import FastAPI
from pydantic import BaseModel

from goban_server_fastapi.auth import PBKDF2PasswordHasher
from goban_server_fastapi.models import get_user

class LoginRequest(BaseModel):
    username: str
    password: str

app = FastAPI()


@app.post('/api/login')
def login(login: LoginRequest):
    print('try dat login', login)
    u = get_user(login.username)
    if u is not None:
        print(u, u.username, u.password)
        hasher = PBKDF2PasswordHasher()
        if hasher.verify(login.password, u.password):
            return 'yes'
    return 'no'

@app.get("/")
def read_root():
    return {"Hello": "World"}


@app.get("/items/{item_id}")
def read_item(item_id: int, q: Union[str, None] = None):
    return {"item_id": item_id, "q": q}