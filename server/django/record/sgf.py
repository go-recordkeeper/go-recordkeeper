from sgfmill import sgf

from record.models import Record


def export_sgf(record: Record) -> bytes:
    game = sgf.Sgf_game(size=record.board_size)
    root_node = game.get_root()
    if record.name:
        root_node.set('GN', record.name)
    if record.comment:
        root_node.set('GC', record.name)
    root_node.set('PB', record.black_player)
    root_node.set('PW', record.white_player)
    root_node.set('KM', record.komi)
    root_node.set('HA', record.handicap)

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
