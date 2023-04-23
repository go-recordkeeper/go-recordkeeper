import sys

import pytest


def run():
    return pytest.main(["goban_server_test"] + sys.argv[1:])
