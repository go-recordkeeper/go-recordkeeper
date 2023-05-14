import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";

register("GET", "/api/user/", async (request) => {
  const id = await getUserId(request);
  const selection =
    await sql`SELECT username, email, is_active FROM auth_user WHERE id=${id}`;
  if (selection.length != 1) {
    return new Response("Failed to authenticate", { status: 403 });
  }
  const { username, email } = selection[0];

  return new Response(JSON.stringify({ id, username, email }));
});
