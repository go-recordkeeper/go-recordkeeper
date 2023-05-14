FROM denoland/deno:debian-1.33.2

EXPOSE 8000
WORKDIR /opt/deno

# Cache the dependencies
COPY ./src/deps.ts /opt/deno/src/deps.ts
RUN deno cache src/deps.ts

COPY deno.* /opt/deno/
COPY ./src /opt/deno/src


CMD ["run", "--allow-net", "--allow-env", "src/main.ts"]
