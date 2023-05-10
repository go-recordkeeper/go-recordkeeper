import { register } from "/router.ts";
import { sql } from "/db.ts";
import { ajv, json_schema as J } from "/deps.ts";

// type LoginRequest = { username: string; password: string };
const LR = J.struct({ username: J.string(), password: J.string() });

const validate = ajv.compile(J.print(LR));

register(
  "POST",
  "/api/login/",
  async (request, params) => {
    let json: J.TypeOf<typeof LR> = await request.json();
    console.log(json);
    console.log(validate(json));
    let x = await sql`SELECT id, password, is_active FROM auth_user;`;
    console.log(x);
    return new Response("aaa", { status: 200 });
  },
);
