"""
The definition of the base FastAPI instance.

All endpoints should import `app` from this module and register using the decorator.
They should also import `User` and `jwt_user` from here to avoid explicit inter-slice dependencies.
This is a convenient central location for anything that many endpoints will need.
"""
from fastapi import Depends, FastAPI
from fastapi.exceptions import RequestValidationError
from starlette import status
from starlette.requests import Request
from starlette.responses import JSONResponse

from goban_server_fastapi.db import DbClient


async def request_validation_exception_handler(
    request: Request, exc: RequestValidationError
) -> JSONResponse:
    fields = {}
    for error in exc.errors():
        field = error["loc"][1]
        if field not in fields:
            fields[field] = []
        fields[field].append(error["msg"])
    return JSONResponse(
        status_code=status.HTTP_400_BAD_REQUEST,
        content=fields,
    )


app = FastAPI(dependencies=[Depends(DbClient)])
app.add_exception_handler(RequestValidationError, request_validation_exception_handler)
