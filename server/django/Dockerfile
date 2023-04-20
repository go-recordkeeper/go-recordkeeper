FROM python:3
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
RUN apt-get update \
    && apt-get install -y postgresql-client \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /goban
COPY setup.py /goban/
# Install the application and also uvicorn
RUN pip install --no-cache-dir . uvicorn
COPY . /goban/

CMD ["uvicorn", "goban.asgi:application", "--host", "0.0.0.0", "--reload"] 
