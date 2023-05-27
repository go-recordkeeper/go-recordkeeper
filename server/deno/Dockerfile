# FROM denoland/deno:debian-1.33.2
FROM debian:buster-slim as install

RUN apt-get update -y && apt-get upgrade -y
RUN apt-get install -y unzip curl
COPY ./deno_install.sh /opt/deno/deno_install.sh
RUN sh /opt/deno/deno_install.sh

FROM debian:buster-slim

COPY --from=install /root/.deno/bin/deno /bin/deno

EXPOSE 8000
WORKDIR /opt/deno

# Cache the dependencies
COPY ./src/deps.ts /opt/deno/src/deps.ts
RUN deno cache src/deps.ts

COPY deno.* /opt/deno/
COPY ./src /opt/deno/src
COPY main.ts /opt/deno/main.ts


CMD ["run", "--allow-net", "--allow-env", "main.ts"]
