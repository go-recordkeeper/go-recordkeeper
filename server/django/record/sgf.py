from sgfmill import sgf

from record.models import Record


def export_sgf(record: Record) -> bytes:
    game = sgf.Sgf_game(size=record.board_size)
    for move in record.moves.all():
        node = game.extend_main_sequence()
        # position increments left to right, then top to bottom
        # sgfmill considers (0,0) to be the lower left corner
        # Therefore, we need to invert y
        x = move.position % record.board_size
        y = record.board_size - (move.position // record.board_size) - 1
        # and also flip x/y??? whatever dude
        node.set_move(move.color.lower(), (y, x))
    return game.serialise()
