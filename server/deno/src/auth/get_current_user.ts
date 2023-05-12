import { register } from "/router.ts";
import { sql } from "/db.ts";
import { verifyJwt } from "/auth/util.ts";

register("GET", "/api/user/", async (request) => {
  const header = request.headers.get("Authorization");
  if (!header || !header.startsWith("Bearer ")) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const token = header.replace("Bearer ", "");
  const sub = await verifyJwt(token);
  if (!sub) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const id = parseInt(sub, 10);
  const selection =
    await sql`SELECT username, email, is_active FROM auth_user WHERE id=${id}`;
  if (selection.length != 1) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const { username, email } = selection[0];

  return new Response(JSON.stringify({ id, username, email }));
});
