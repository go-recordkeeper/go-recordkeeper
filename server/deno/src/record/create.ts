import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { getUserId } from "/auth/util.ts";

const CreateRequest = J.union(
  J.struct({
    board_size: J.number(),
  }),
)(
  J.partial({
    name: J.string(),
    black_player: J.string(),
    white_player: J.string(),
    comment: J.string(),
    handicap: J.number(),
    komi: J.number(),
    ruleset: J.string(),
  }),
);
const body = validator(CreateRequest);
const defaults = {
  name: "",
  black_player: "Black",
  white_player: "White",
  comment: "",
  handicap: 0,
  komi: 7.5,
  ruleset: "AGA",
};

register("POST", "/api/records/", async (request) => {
  const userId = await getUserId(request);
  const {
    board_size,
    name,
    black_player,
    white_player,
    comment,
    handicap,
    komi,
    ruleset,
  } = { ...defaults, ...await body(request) };
  if (![9, 13, 19].includes(board_size)) {
    return new Response("Invalid board size", { status: 400 });
  }
  const winner = "U";
  const now = new Date();
  const rows = await sql`INSERT INTO record_record
  (owner_id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created)
  VALUES
  (${userId}, ${board_size}, ${name}, ${black_player}, ${white_player}, ${comment}, ${handicap}, ${komi}, ${ruleset}, ${winner}, ${now})
  RETURNING id`;
  const recordId = Number.parseInt(rows[0].id, 10);
  return new Response(
    JSON.stringify({
      id: recordId,
      owner: userId,
      board_size,
      created: now,
      name,
      black_player,
      white_player,
      comment,
      handicap,
      komi,
      ruleset,
      winner,
    }),
    { status: 201 },
  );
});
