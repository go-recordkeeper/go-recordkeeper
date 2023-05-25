import { register } from "/router.ts";
import { sql } from "/db.ts";
import { getUserId } from "/auth/util.ts";
import { Board } from "/go.ts";
import { format } from "/deps.ts";

register(
  "GET",
  "/api/records/{id}/download/",
  async (request, { id: recordId }) => {
    const userId = await getUserId(request);
    const rows =
      await sql`SELECT board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = ${userId} AND id = ${recordId}`;
    if (rows.length == 0) {
      return new Response("Not found", { status: 404 });
    }
    const {
      board_size,
      black_player,
      white_player,
      handicap,
      komi,
      _ruleset,
      created,
    } = rows[0];
    let { name, comment, winner } = rows[0];
    if (name !== "") {
      name = `GN[${name}]`;
    }
    if (comment !== "") {
      comment = `GC[${comment}]`;
    }
    if (winner == "U") {
      winner = "Void";
    } else if (winner == "B") {
      winner = "B+R";
    } else if (winner == "W") {
      winner = "W+R";
    } else {
      return new Response("Error parsing the winner", { status: 500 });
    }
    const board = new Board(board_size);
    const moves =
      await sql`SELECT position, color FROM record_move WHERE record_id=${recordId} ORDER BY move ASC`;
    const formattedMoves = moves.map(({ position, color }) => {
      let chars: string;
      if (position !== null) {
        const [x, y] = board.toCoord(position);
        chars = `${String.fromCharCode(x + 97)}${String.fromCharCode(y + 97)}`;
      } else {
        chars = "tt";
      }
      return `${color}[${chars}]`;
    }).join(";");
    const contents =
      `(;FF[4]CA[UTF-8]${comment}GM[1]${name}HA[${handicap}]KM[${komi}]PB[${black_player}]PW[${white_player}]RE[${winner}]SZ[${board_size}];${formattedMoves})`;
    const date_string = format(created, "yyyy_MM_dd");
    let filename: string;
    if (name !== "") {
      filename = `${name}_${date_string}.sgf`;
    } else {
      filename = `${black_player}_vs_${white_player}_${date_string}.sgf`;
    }
    return new Response(contents, {
      headers: new Headers({
        "content-type": "application/x-go-sgf",
        "content-disposition": `attachment; filename="${filename}"`,
      }),
    });
  },
);
