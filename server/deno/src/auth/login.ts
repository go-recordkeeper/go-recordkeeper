import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";

// type LoginRequest = { username: string; password: string };
const LoginRequest = J.struct({ username: J.string(), password: J.string() });
const body = validator(LoginRequest);

register(
  "POST",
  "/api/login/",
  async (request, params) => {
    let json = await body(request);
    let x = await sql`SELECT id, password, is_active FROM auth_user;`;
    console.log(x);
    return new Response("aaa", { status: 200 });
  },
);
