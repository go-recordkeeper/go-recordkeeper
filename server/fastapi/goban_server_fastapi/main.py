from typing import Union

from fastapi import FastAPI
from fastapi.exceptions import RequestValidationError
from pydantic import BaseModel
from starlette import status
from starlette.requests import Request
from starlette.responses import JSONResponse, Response

from goban_server_fastapi.auth import PBKDF2PasswordHasher, generate_token
from goban_server_fastapi.models import get_user

class LoginRequest(BaseModel):
    username: str
    password: str

async def request_validation_exception_handler(
    request: Request, exc: RequestValidationError
) -> JSONResponse:
    print(exc.errors())
    fields = {}
    for error in exc.errors():
        field = error['loc'][1]
        if field not in fields:
            fields[field] = []
        fields[field].append(error['msg'])
    return JSONResponse(
        status_code=status.HTTP_400_BAD_REQUEST,
        content=fields,
    )

app = FastAPI()
app.add_exception_handler(RequestValidationError, request_validation_exception_handler)


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
