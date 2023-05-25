import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";
import { Board, Color, Coord, deserializeMoves, GoError, Move } from "/go.ts";

register(
  "POST",
  "/api/records/{recordId}/undo/",
  async (request, { recordId }) => {
    const userId = await getUserId(request);
    const records =
      await sql`SELECT board_size FROM record_record WHERE owner_id=${userId} AND id=${recordId}`;
    if (records.length != 1) {
      return new Response("Not Found", { status: 404 });
    }
    const { board_size } = records[0];
    const moves =
      await sql`SELECT position, color, move FROM record_move WHERE record_id=${recordId} ORDER BY move ASC`;
    if (moves.length == 0) {
      return new Response("No moves to undo", { status: 403 });
    }
    // deno-lint-ignore no-explicit-any
    const { position, move } = moves[moves.length - 1] as any;
    let response: string;
    if (position === null) {
      // Undoing a pass, board state does not change
      response = JSON.stringify({ add: [], remove: [] });
    } else {
      const board = new Board(board_size);

      let allCaptures: [Move, Coord[]][];
      try {
        allCaptures = board.playMoves(deserializeMoves(board, moves));
      } catch (e) {
        if (e instanceof GoError) {
          return new Response(e.message, { status: 403 });
        }
        throw e;
      }
      const [[coord, moveColor], captures] = allCaptures.pop() as [
        Move,
        Coord[],
      ];
      let captureColor: Color;
      if (moveColor == Color.Black) {
        captureColor = Color.White;
      } else if (moveColor == Color.White) {
        captureColor = Color.Black;
      }
      const [x, y] = coord as Coord;
      response = JSON.stringify({
        add: captures.map(([x, y]) => ({
          x,
          y,
          color: Color.toString(captureColor),
        })),
        remove: [{ x, y }],
      });
    }
    await sql`DELETE FROM record_move WHERE record_id=${recordId} AND move=${move}`;
    return new Response(response);
  },
);
