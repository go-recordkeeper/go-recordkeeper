import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { createJwt, hashPassword } from "/auth/util.ts";

const LoginRequest = J.struct({ username: J.string(), password: J.string() });
const body = validator(LoginRequest);

register(
  "POST",
  "/api/login/",
  async (request) => {
    const { username, password } = await body(request);
    // This is a dumb looking way to organize things, but we don't want to fail early to avoid leaking information to timing attacks

    // not a valid identifier
    let id = -1;
    let db_password = "pbkdf2_sha256$390000$notvalidsalt$notvalidhash";
    const selection =
      await sql`SELECT id, password, is_active FROM auth_user WHERE username=${username};`;
    if (selection.length == 1) {
      ({ id, password: db_password } = selection[0]);
    }

    let salt = "not valid salt";
    let db_hash = "not valid hash";
    const match = db_password.match(
      /^pbkdf2_sha256\$([0-9]+)\$([a-zA-Z0-9+/]+)\$([a-zA-Z0-9+/=]+)$/,
    );
    if (match) {
      salt = match[2];
      db_hash = match[3];
    } else {
      console.error("Error parsing password field", username);
    }

    const hashed_password = await hashPassword(password, salt);
    if (hashed_password == db_hash) {
      const jwt = await createJwt(id);
      return new Response(`"${jwt}"`, { status: 200 });
    } else {
      return new Response("", { status: 401 });
    }
  },
);
