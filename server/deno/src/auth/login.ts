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
    const json = await body(request);
    const x =
      await sql`SELECT id, password, is_active FROM auth_user WHERE username=${json.username};`;
    console.log(x);
    const dbthingy: string = x[0].password;
    const match = dbthingy.match(
      /^pbkdf2_sha256\$([0-9]+)\$([a-zA-Z0-9+/]+)\$([a-zA-Z0-9+/=]+)$/,
    );
    if (!match) {
      // TODO
      throw "bad juju";
    }
    const [_, iterations, salt, hash] = match;
    const passkey: CryptoKey = await crypto.subtle.importKey(
      "raw",
      new TextEncoder().encode(json.password),
      { name: "PBKDF2" },
      false,
      ["deriveKey", "deriveBits"],
    );
    const kk = await crypto.subtle.deriveKey(
      {
        name: "PBKDF2",
        salt: new TextEncoder().encode(salt),
        iterations: parseInt(iterations, 10),
        hash: "SHA-256",
      },
      passkey,
      { name: "HMAC", hash: "SHA-256", length: 32 * 8 },
      true,
      ["sign", "verify"],
    );
    const out = await crypto.subtle.exportKey("raw", kk);
    const buf2b64 = (buffer: ArrayBuffer) =>
      self.btoa(
        Array.prototype.map.call(
          new Uint8Array(buffer),
          (x) => String.fromCharCode(x),
        ).join(""),
      );
    console.log("computed", buf2b64(out));
    console.log("from db ", hash);
    return new Response("" + (buf2b64(out) == hash), { status: 200 });
  },
);
