import { register } from "/router.ts";
import { postgres } from "/deps.ts";

// Deno.env.get("POSTGRES_USER")
// const sql = postgres("postgres://username:password@host:port/database");

register(
  "POST",
  "/api/login/",
  async (request, params) => {
    console.log("fooo", await request.json());
    return new Response("aaa", { status: 200 });
  },
);
