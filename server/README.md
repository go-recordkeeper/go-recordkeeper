# Server Implementations

This is where all the server implementations live.

## Architecture
Each implementation is a web application that exposes a REST API and reads/writes data to/from a Postgres DB. The details of that implementation are highly dependent on the language and frameworks chosen.

All implementations are dockerized, and therefore have a `Dockerfile` and a `docker-compose.yml`. The `Dockerfile` is used to build the Docker image used for development, integration testing, and deployment. The `docker-compose.yml` is used to manage the development environment, specifically the Postgres DB and the Django container used for migrations. 
