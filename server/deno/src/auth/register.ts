import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";

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
    const saltkey = await crypto.subtle.generateKey(
      { name: "HMAC", hash: "SHA-256", length: 16 * 6 },
      true,
      [
        "sign",
        "verify",
      ],
    );
    const buf2b64 = (buffer: ArrayBuffer) =>
      self.btoa(
        Array.prototype.map.call(
          new Uint8Array(buffer),
          (x) => String.fromCharCode(x),
        ).join(""),
      );
    const salt = buf2b64(await crypto.subtle.exportKey("raw", saltkey));
    // const saltkeyagain = await crypto.subtle.importKey(
    //   "raw",
    //   new TextEncoder().encode(salt),
    //   { name: "PBKDF2" },
    //   false,
    //   ["deriveKey", "deriveBits"],
    // );
    const passkey: CryptoKey = await crypto.subtle.importKey(
      "raw",
      new TextEncoder().encode(password),
      { name: "PBKDF2" },
      false,
      ["deriveKey", "deriveBits"],
    );
    const kk = await crypto.subtle.deriveKey(
      {
        name: "PBKDF2",
        salt: new TextEncoder().encode(salt),
        iterations: 390000,
        hash: "SHA-256",
      },
      passkey,
      { name: "HMAC", hash: "SHA-256", length: 32 * 8 },
      true,
      ["sign", "verify"],
    );
    const hash = await crypto.subtle.exportKey("raw", kk);
    const now = new Date();
    const pppp = `pbkdf2_sha256$390000$${salt}$${buf2b64(hash)}`;
    console.log(pppp);
    try {
      const rrr =
        await sql`INSERT INTO auth_user (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active) VALUES (${username}, ${email}, ${pppp}, ${now}, ${now}, '', '', false, false, true) RETURNING id`;
      console.log(rrr);
      return new Response(
        JSON.stringify({
          id: rrr[0].id,
          username: username,
          email: email,
        }),
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
