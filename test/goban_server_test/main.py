import sys

import pytest


def run():
    print(pytest.main(["goban_server_test"] + sys.argv[1:]))
