from enum import Enum


class IllegalMoveError(Exception):
    pass


class Stone(Enum):
    BLACK = 'B'
    WHITE = 'W'
    NONE = ' '


class Board:
    def __init__(self, size):
        # self.matrix = [[Stone.NONE] * size] * size
        self.size = size
        self.moves = {}
        self.group_index = {}
        self.groups = set()
        self.removals: list[list[tuple[int, int]]] = []

    def place_stone(self, stone: Stone, x: int, y: int) -> list[tuple[int, int]]:
        if (x, y) in self.moves:
            raise IllegalMoveError('space is already occupied')
        self.moves[(x, y)] = stone
        # This is the new group the new stone will belong to
        group = set()
        group.add((x, y))
        # Add any adjacent groups to the new group
        for adj in self._adjacents((x, y)):
            if adj in self.group_index and self.moves[adj] == stone:
                group = group.union(self.group_index[adj])
                # Remove the old subgroup from the groups list
                try:
                    self.groups.remove(self.group_index[adj])
                except KeyError:
                    # If the old group was touching the new stone in two places, it's already gone
                    pass
        # Reassign the group index of all adjacent groups so they all refer to the same set
        for point in group:
            self.group_index[point] = group
        # Add the newly minted group to the groups set
        self.groups.add(frozenset(group))

        removals = []
        # Check if any adjacent groups of the opposite color were killed
        for adj in self._adjacents((x, y)):
            if (
                adj in self.group_index
                and self.moves[adj] != stone
                and self._is_dead(self.group_index[adj])
            ):
                removals.extend(self._remove_group(self.group_index[adj]))

        # Check if the move was actually suicidal
        if self._is_dead(self.group_index[(x, y)]):
            raise IllegalMoveError('move is suicidal')

        removals = sorted(removals, key=lambda t: t[0] + t[1] * 100)
        self.removals.append(removals)
        return removals

    def pass_turn(self):
        self.removals.append([])

    def _remove_group(self, group: set[tuple[int, int]]):
        for point in group:
            del self.moves[point]
            del self.group_index[point]
            yield point
        self.groups.remove(group)

    def _is_dead(self, group):
        for point in group:
            for adj in self._adjacents(point):
                if adj not in self.moves:
                    return False
        return True

    def _adjacents(self, point):
        x, y = point
        if x > 0:
            yield (x - 1, y)
        if x < self.size - 1:
            yield (x + 1, y)
        if y > 0:
            yield (x, y - 1)
        if y < self.size - 1:
            yield (x, y + 1)
