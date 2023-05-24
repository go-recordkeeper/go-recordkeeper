import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";
import { Board, Color, Move } from "/go.ts";

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
  const board = new Board(board_size);
  const moves: Move[] = rows.map((row) => {
    if (row.position === null) {
      return [null, Color.fromString(row.color)];
    }
    return [board.toCoord(row.position), Color.fromString(row.color)];
  });
  const movesAndCaptures = board.playMoves(moves);
  const stones = [];
  for (let pos = 0; pos < board_size * board_size; pos += 1) {
    const color = board.stones[pos];
    if (color !== null) {
      const [x, y] = board.toCoord(pos);
      stones.push({ x, y, color: Color.toString(color) });
    }
  }
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
      moves: movesAndCaptures.map(([[coord, color], captures]) => ({
        position: (coord !== null) ? { x: coord[0], y: coord[1] } : null,
        color: Color.toString(color),
        captures: captures.map(([x, y]) => ({ x, y })),
      })),
      stones,
    }),
  );
});
