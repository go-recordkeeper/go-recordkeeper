import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { getUserId } from "/auth/util.ts";

register("GET", "/api/records/{id}/", async (request, { id: recordId }) => {
  const userId = await getUserId(request);
  let rows =
    await sql`SELECT board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record 
  WHERE
  owner_id=${userId} AND id=${recordId}`;
  if (rows.length == 0) {
    return new Response("Not found", { status: 404 });
  }
  const {
    board_size,
    created,
    name,
    black_player,
    white_player,
    comment,
    handicap,
    komi,
    ruleset,
    winner,
  } = rows[0];
  rows =
    await sql`SELECT position, color FROM record_move WHERE record_id=${recordId} ORDER BY move ASC`;
  console.log("mmoves", rows);
  return new Response(
    JSON.stringify({
      id: Number.parseInt(recordId, 10),
      owner: userId,
      board_size,
      created,
      name,
      black_player,
      white_player,
      comment,
      handicap,
      komi,
      ruleset,
      winner,
      moves: [],
      stones: [],
    }),
  );
});
