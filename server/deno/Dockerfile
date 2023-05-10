FROM denoland/deno:debian-1.33.2

EXPOSE 8000
WORKDIR /opt/deno
COPY deno.* /opt/deno/
COPY ./src /opt/deno/src

RUN deno cache src/main.ts

CMD ["run", "--allow-net", "--allow-env", "src/main.ts"]
