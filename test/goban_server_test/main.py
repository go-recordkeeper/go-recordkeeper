import sys

import pytest


def run():
    # print(sys.argv[1:])
    # process = Popen(["docker", "compose", "run", "--rm", "django", "python", "manage.py", "migrate"])
    # print(process.wait())
    # Invoke pytest with any arguments passed
    print(pytest.main(["goban_server_test"] + sys.argv[1:]))
