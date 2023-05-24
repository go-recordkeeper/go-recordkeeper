import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";

register(
  "DELETE",
  "/api/records/{recordId}/",
  async (request, { recordId }) => {
    const userId = await getUserId(request);
    // Verify the record exists before doing any deletes
    const records =
      await sql`SELECT id FROM record_record WHERE owner_id=${userId} AND id=${recordId}`;
    if (records.length != 1) {
      return new Response("Not Found", { status: 404 });
    }
    // Delete the moves first because Django didn't set up cascading deletes properly
    await sql`DELETE FROM record_move WHERE record_id=${recordId}`;
    await sql`DELETE FROM record_record WHERE owner_id=${userId} AND id=${recordId}`;
    return new Response(null, { status: 204 });
  },
);
