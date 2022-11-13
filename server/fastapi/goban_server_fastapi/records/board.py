from typing import Iterator, Literal

from fastapi import HTTPException

Color = Literal["B"] | Literal["W"]


class SpaceAlreadyOccupiedError(HTTPException):
    def __init__(self):
        super().__init__(status_code=403, detail="Point is already occupied")


class SuicidalMoveError(HTTPException):
    def __init__(self):
        super().__init__(status_code=403, detail="Move is suicidal")


class BoardState:
    size: int
    stones: dict[tuple[int, int], Color]

    def __init__(self, size: int):
        self.size = size
        self.stones = {}

    def play_move(self, x: int, y: int, color: Color) -> list[tuple[int, int]]:
        """Play a move onto the board and return the positions of all captured stones."""
        if (x, y) in self.stones:
            raise SpaceAlreadyOccupiedError()
        self.stones[(x, y)] = color
        captures = set()
        # For every point adjacent to the placed stone,
        # if the stone is of the opposite color,
        # has not yet been marked for capture,
        # and is in a group with no liberties,
        # mark it for capture.
        for adjacent in self._adjacents((x, y)):
            if (
                adjacent in self.stones
                and self.stones[adjacent] != color
                and adjacent not in captures
            ):
                group, liberties = self._build_group(adjacent)
                if liberties == 0:
                    captures = captures.union(group)

        # If nothing was captured, maybe it was a suicidal move.
        if len(captures) == 0:
            _group, liberties = self._build_group((x, y))
            if liberties == 0:
                # Remove the erroneous move before raising the exception
                del self.stones[(x, y)]
                raise SuicidalMoveError()

        # Remove any captured stones
        for capture in captures:
            del self.stones[capture]

        return sorted(captures)

    def _adjacents(self, coord: tuple[int, int]) -> Iterator[tuple[int, int]]:
        (x, y) = coord
        if x > 0:
            yield (x - 1, y)
        if y > 0:
            yield (x, y - 1)
        if x < self.size - 1:
            yield (x + 1, y)
        if y < self.size - 1:
            yield (x, y + 1)

    def _build_group(
        self, initial_coord: tuple[int, int]
    ) -> tuple[set[tuple[int, int]], int]:
        """Given an initial stone that is part of group, return the positions of all stones in the group, and the number of liberties the group has."""
        color = self.stones[initial_coord]
        to_visit = set([initial_coord])
        visited = set()
        liberties = set()
        while len(to_visit) > 0:
            coord = to_visit.pop()
            visited.add(coord)
            for adjacent in self._adjacents(coord):
                if adjacent not in self.stones:
                    liberties.add(adjacent)
                elif self.stones[adjacent] == color and adjacent not in visited:
                    to_visit.add(adjacent)
        return (visited, len(liberties))
