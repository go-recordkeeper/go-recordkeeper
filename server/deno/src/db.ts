import { postgres } from "/deps.ts";

// const GOBAN_DEVELOPMENT = Deno.env.get("GOBAN_DEVELOPMENT") || "";
const POSTGRES_NAME = Deno.env.get("POSTGRES_NAME") || "";
const POSTGRES_USER = Deno.env.get("POSTGRES_USER") || "";
const POSTGRES_PASSWORD = Deno.env.get("POSTGRES_PASSWORD") || "";
const POSTGRES_HOST = Deno.env.get("POSTGRES_HOST") || "";

export const sql = postgres(
  `postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@${POSTGRES_HOST}:5432/${POSTGRES_NAME}`,
);
