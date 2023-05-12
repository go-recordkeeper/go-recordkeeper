import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { djwt, json_schema as J } from "/deps.ts";

register("GET", "/api/user/", async (request) => {
  const header = request.headers.get("Authorization");
  if (!header || !header.startsWith("Bearer ")) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const token = header.replace("Bearer ", "");
  const secret_key = Deno.env.get("GOBAN_SECRET_KEY") ||
    "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj";
  const jwtkey = await crypto.subtle.importKey(
    "raw",
    new TextEncoder().encode(secret_key),
    { name: "HMAC", hash: "SHA-512" },
    true,
    ["sign", "verify"],
  );
  const { sub } = await djwt.verify(token, jwtkey, {
    audience: "go-recordkeeper",
  });
  if (!sub) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const id = parseInt(sub, 10);
  const xxx =
    await sql`SELECT username, email, is_active FROM auth_user WHERE id=${id}`;
  console.log(xxx);
  if (xxx.length != 1) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const { username, email } = xxx[0];

  return new Response(JSON.stringify({ id, username, email }));
});
