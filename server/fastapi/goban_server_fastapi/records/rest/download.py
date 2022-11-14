from fastapi import Depends, HTTPException
from fastapi.responses import StreamingResponse
from sgfmill import sgf
from sqlalchemy import asc, select

from goban_server_fastapi.auth import User, jwt_user
from goban_server_fastapi.db import DbClient
from goban_server_fastapi.records.models import Move, Record, next_color
from goban_server_fastapi.rest import app


@app.get("/api/records/{record_id}/download/", status_code=200)
def download(
    record_id: int,
    db: DbClient = Depends(),
    current_user: User = Depends(jwt_user),
):
    with db.session() as session:
        record: Record = session.scalar(
            select(Record)
            .where(Record.id == record_id)
            .where(Record.owner_id == current_user.id)
        )
        if record is None:
            raise HTTPException(status_code=404)
        moves = session.scalars(
            select(Move).where(Move.record_id == record.id).order_by(asc(Move.move))
        )

        game = sgf.Sgf_game(size=record.board_size)
        root_node = game.get_root()
        if record.name:
            root_node.set("GN", record.name)
        if record.comment:
            root_node.set("GC", record.comment)
        root_node.set("PB", record.black_player)
        root_node.set("PW", record.white_player)
        root_node.set("KM", record.komi)
        root_node.set("HA", record.handicap)
        if record.winner != "U":
            root_node.set("RE", f"{record.winner}+R")
        else:
            root_node.set("RE", "Void")

        for move in moves:
            node = game.extend_main_sequence()
            # position increments left to right, then top to bottom
            # sgfmill considers (0,0) to be the lower left corner
            # Therefore, we need to invert y
            if move.position is not None:
                x = move.position % record.board_size
                y = record.board_size - (move.position // record.board_size) - 1
                # and also flip x/y??? whatever dude
                node.set_move(move.color.lower(), (y, x))
            else:
                node.set_move(move.color.lower(), None)

        date = record.created.strftime(r"%Y_%m_%d")
        if record.name:
            filename = f"{record.name}_{date}.sgf"
        else:
            black = record.black_player.replace(" ", "_")
            white = record.white_player.replace(" ", "_")
            filename = f"{black}_vs_{white}_{date}.sgf"

        def _serialize_game():
            yield game.serialise()

        return StreamingResponse(
            _serialize_game(),
            media_type="application/x-go-sgf",
            headers={"Content-Disposition": f'attachment; filename="{filename}"'},
        )
