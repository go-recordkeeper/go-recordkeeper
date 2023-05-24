import { register, validator } from "/router.ts";
import { sql } from "/db.ts";
import { json_schema as J } from "/deps.ts";
import { getUserId } from "/auth/util.ts";
import { Board, Color, Coord, GoError } from "/go.ts";

const PlayRequest = J.struct({
  x: J.number(),
  y: J.number(),
});
const body = validator(PlayRequest);

register(
  "POST",
  "/api/records/{recordId}/play/",
  async (request, { recordId }) => {
    const userId = await getUserId(request);
    const { x, y } = await body(request);
    const records =
      await sql`SELECT board_size, handicap FROM record_record WHERE owner_id=${userId} AND id=${recordId}`;
    if (records.length != 1) {
      return new Response("Not Found", { status: 404 });
    }
    const { board_size, handicap } = records[0];
    const moves =
      await sql`SELECT position, color FROM record_move WHERE record_id=${recordId} ORDER BY move ASC`;
    let color: Color;
    if (moves.length == 0 || moves.length < handicap) {
      // First move is always black
      // If we are still in the handicap phase, next move is black
      color = Color.Black;
    } else if (moves[moves.length - 1].color == "W") {
      // If the last move was white, the next move is black
      color = Color.Black;
    } else {
      color = Color.White;
    }
    const board = new Board(board_size);
    let captures: Coord[];
    try {
      board.playMoves(
        moves.map((
          row,
        ) => [board.toCoord(row.position), Color.fromString(row.color)]),
      );
      captures = board.playMove([[x, y], color]);
    } catch (e) {
      if (e instanceof GoError) {
        return new Response(e.message, { status: 403 });
      }
      throw e;
    }
    const move_number = moves.length + 1;
    const pos = board.toPos([x, y]);
    await sql`INSERT INTO record_move
      (record_id, position, color, move)
      VALUES
      (${recordId}, ${pos}, ${Color.toString(color)}, ${move_number})`;
    return new Response(
      JSON.stringify({
        add: [{ x, y, color: Color.toString(color) }],
        remove: captures.map(([x, y]) => ({ x, y })),
      }),
      {
        status: 201,
      },
    );
  },
);
