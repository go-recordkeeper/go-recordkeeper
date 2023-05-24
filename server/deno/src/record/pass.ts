import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";
import { Color } from "/go.ts";

register(
  "POST",
  "/api/records/{recordId}/pass/",
  async (request, { recordId }) => {
    const userId = await getUserId(request);
    const records =
      await sql`SELECT handicap FROM record_record WHERE owner_id=${userId} AND id=${recordId}`;
    if (records.length != 1) {
      return new Response("Not Found", { status: 404 });
    }
    const { handicap } = records[0];
    const moves =
      await sql`SELECT color FROM record_move WHERE record_id=${recordId} ORDER BY move ASC`;
    const colors = moves.map((row) => Color.fromString(row.color));
    let color: Color;
    if (colors.length == 0 || colors.length < handicap) {
      // First move is always black
      // If we are still in the handicap phase, next move is black
      color = Color.Black;
    } else if (colors[colors.length - 1] == Color.White) {
      // If the last move was white, the next move is black
      color = Color.Black;
    } else {
      color = Color.White;
    }
    const move_number = colors.length + 1;
    await sql`INSERT INTO record_move
      (record_id, position, color, move)
      VALUES
      (${recordId}, NULL, ${Color.toString(color)}, ${move_number})`;
    return new Response(null, { status: 201 });
  },
);
