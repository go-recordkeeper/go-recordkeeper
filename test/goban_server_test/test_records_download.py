from anys import ANY_DATETIME_STR
from sgfmill import sgf


def test_download(user_client, record, move_factory):
    # Set up two moves and two passes
    move_factory(0, 0)
    move_factory(8, 0)
    user_client.post(f"/api/records/{record['id']}/pass/")
    user_client.post(f"/api/records/{record['id']}/pass/")

    response = user_client.get(f"/api/records/{record['id']}/download/")
    assert response.status_code == 200
    assert response.headers["Content-Type"] == "application/x-go-sgf"
    assert (
        response.headers["Content-Disposition"]
        == f'attachment; filename="Black_vs_White_{record["created"]}.sgf"'
    )

    game = sgf.Sgf_game.from_bytes(response.content)
    # assert game.root.get("GN") == record.name
    # assert game.root.get("GC") == record.comment
    print(response.content)
    assert game.get_player_name("b") == record["black_player"]
    assert game.get_player_name("w") == record["white_player"]
    assert game.get_komi() == record["komi"]
    assert game.get_handicap() == (
        record["handicap"] if record["handicap"] > 0 else None
    )
    assert game.get_winner() == None
    assert [node.get_move() for node in game.get_main_sequence()] == [
        (None, None),
        ("b", (8, 0)),
        ("w", (8, 8)),
        ("b", None),
        ("w", None),
    ]
