import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { generateSalt, hashPassword } from "/auth/util.ts";

const RegisterRequest = J.struct({
  email: J.string(),
  username: J.string(),
  password: J.string(),
});
const body = validator(RegisterRequest);

register(
  "POST",
  "/api/register/",
  async (request) => {
    const { email, username, password } = await body(request);
    // Email must look like "foo@bar.baz"
    if (!email.match(/.*@.*\..*/)) {
      return new Response("Invalid email.", {
        status: 400,
      });
    }
    const salt = await generateSalt();
    const hash = await hashPassword(password, salt);
    const now = new Date();
    const formatted_hash = `pbkdf2_sha256$390000$${salt}$${hash}`;
    try {
      const [{ id }] =
        await sql`INSERT INTO auth_user (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active) VALUES (${username}, ${email}, ${formatted_hash}, ${now}, ${now}, '', '', false, false, true) RETURNING id`;
      return new Response(
        JSON.stringify({ id, username, email }),
        { status: 201 },
      );
    } catch (e) {
      if (Object.getPrototypeOf(e).code == 23505) {
        return new Response("A user with that username already exists.", {
          status: 400,
        });
      } else {
        throw e;
      }
    }
  },
);
