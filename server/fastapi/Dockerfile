FROM python:3
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

RUN pip install "poetry==1.2.1"

WORKDIR /goban
COPY poetry.lock pyproject.toml /goban/

# production also needs --no-dev
RUN poetry config virtualenvs.create false \
  && poetry install --no-interaction --no-ansi --no-root

ENTRYPOINT uvicorn goban_server_fastapi.main:app --host 0.0.0.0 --reload
