import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { getUserId } from "/auth/util.ts";

const UpdateRequest = J.struct({
  name: J.string(),
  black_player: J.string(),
  white_player: J.string(),
  comment: J.string(),
  handicap: J.number(),
  komi: J.number(),
  ruleset: J.string(),
  winner: J.string(),
});
const body = validator(UpdateRequest);

register("PUT", "/api/records/{id}/", async (request, { id: recordId }) => {
  const userId = await getUserId(request);
  const {
    name,
    black_player,
    white_player,
    comment,
    handicap,
    komi,
    ruleset,
    winner,
  } = await body(request);
  const rows = await sql`UPDATE record_record SET
  name=${name}, black_player=${black_player}, white_player=${white_player}, comment=${comment}, handicap=${handicap}, komi=${komi}, ruleset=${ruleset}, winner=${winner}
  WHERE
  owner_id=${userId} AND id=${recordId}
  RETURNING board_size, created`;
  const board_size = Number.parseInt(rows[0].board_size, 10);
  const created = rows[0].created;
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
    }),
  );
});
