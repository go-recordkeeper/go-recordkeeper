import { assertEquals, assertThrows } from "/deps.ts";

export enum Color {
  White,
  Black,
}

// deno-lint-ignore no-namespace
export namespace Color {
  export function toString(color: Color) {
    if (color == Color.White) {
      return "W";
    } else {
      return "B";
    }
  }
  export function fromString(str: string) {
    if (str == "W") {
      return Color.White;
    } else if (str == "B") {
      return Color.Black;
    } else {
      throw new Error(`Unknown color string ${str}`);
    }
  }
}

export type Coord = [number, number];

type Pos = number;

type Group = Set<Pos>;

export type Move = [Coord | null, Color];

export function deserializeMoves(
  board: Board,
  // moves: { position: number | null; color: string }[],
  // deno-lint-ignore no-explicit-any
  moves: any[],
): Move[] {
  return moves.map(
    (
      { position, color },
    ) => [
      (position === null) ? null : board.toCoord(position as number),
      Color.fromString(color),
    ],
  );
}

export class GoError extends Error {}
export class OutOfBoundsError extends GoError {}
export class SpaceOccupiedError extends GoError {}
export class SuicideError extends GoError {}

// type Board = (Color | null)[];
export class Board {
  size: number;
  stones: (Color | null)[];
  constructor(size: number) {
    this.size = size;
    this.stones = Array.from({ length: size * size }, () => null);
  }
  toCoord(pos: number): Coord {
    return [pos % this.size, Math.floor(pos / this.size)];
  }
  toPos([x, y]: Coord) {
    return x + (y * this.size);
  }
  *adjacents(pos: Pos) {
    const [x, y] = this.toCoord(pos);
    if (x > 0) {
      yield this.toPos([x - 1, y]);
    }
    if (y > 0) {
      yield this.toPos([x, y - 1]);
    }
    if (x < this.size - 1) {
      yield this.toPos([x + 1, y]);
    }
    if (y < this.size - 1) {
      yield this.toPos([x, y + 1]);
    }
  }

  set(coord: Coord, stone: Color | null) {
    this.stones[this.toPos(coord)] = stone;
  }

  group(coord: Coord): [Coord[], number] {
    const group: Group = new Set();
    const queue: Pos[] = [];
    const liberties: Group = new Set();
    const pos = this.toPos(coord);
    const color = this.stones[pos];
    if (color !== null) {
      group.add(pos);
      queue.push(pos);
    }
    while (queue.length > 0) {
      const nextPos = queue.pop() as number;
      for (const adj of this.adjacents(nextPos)) {
        const adjColor = this.stones[adj];
        if (adjColor === null) {
          liberties.add(adj);
        } else if (
          adjColor === color && !group.has(adj) && !queue.includes(adj)
        ) {
          group.add(adj);
          queue.push(adj);
        }
      }
    }
    return [
      Array.from(group).sort().map((pos) => this.toCoord(pos)),
      liberties.size,
    ];
  }

  playMove([coord, color]: Move) {
    if (coord === null) {
      return [];
    }
    const [x, y] = coord;
    const pos = this.toPos(coord);
    if (x < 0 || x >= this.size || y < 0 || y >= this.size) {
      throw new OutOfBoundsError();
    }
    if (this.stones[pos] !== null) {
      throw new SpaceOccupiedError();
    }
    // Place the stone
    this.stones[pos] = color;
    // Look for kills
    let kills: Coord[] = [];
    for (const adj of this.adjacents(pos)) {
      const [adjGroup, liberties] = this.group(this.toCoord(adj));
      if (liberties === 0) {
        kills = kills.concat(adjGroup);
      }
    }
    // Check for suicide
    if (kills.length === 0) {
      const [_group, liberties] = this.group(coord);
      if (liberties == 0) {
        // We need to undo the stone placement before we throw
        this.stones[pos] = null;
        throw new SuicideError();
      }
    }
    // No suicide, kill the dead stones
    for (const kill of kills) {
      this.stones[this.toPos(kill)] = null;
    }
    kills.sort();
    return kills;
  }

  playMoves(moves: Move[]) {
    const kills: [Move, Coord[]][] = [];
    for (const move of moves) {
      kills.push([move, this.playMove(move)]);
    }
    return kills;
  }
}

Deno.test("group empty", () => {
  const board = new Board(9);
  assertEquals(board.group([0, 0]), [[], 0]);
});

Deno.test("group one stone", () => {
  const board = new Board(9);
  board.set([0, 0], Color.Black);
  assertEquals(board.group([0, 0]), [[[0, 0]], 2]);
});

Deno.test("group two stone", () => {
  const board = new Board(9);
  board.set([1, 1], Color.Black);
  board.set([1, 0], Color.Black);
  assertEquals(board.group([1, 1]), [[[1, 0], [1, 1]], 5]);
});

Deno.test("group in atari", () => {
  const board = new Board(9);
  board.set([0, 0], Color.White);
  board.set([1, 0], Color.Black);
  board.set([2, 0], Color.White);
  assertEquals(board.group([0, 0]), [[[0, 0]], 1]);
  assertEquals(board.group([1, 0]), [[[1, 0]], 1]);
});

Deno.test("group captured", () => {
  const board = new Board(9);
  board.set([0, 0], Color.White);
  board.set([1, 0], Color.Black);
  board.set([2, 0], Color.White);
  board.set([1, 1], Color.White);
  assertEquals(board.group([1, 0]), [[[1, 0]], 0]);
});

Deno.test("group empty triangle", () => {
  const board = new Board(9);
  board.set([2, 2], Color.White);
  board.set([1, 1], Color.White);
  board.set([2, 1], Color.White);
  assertEquals(board.group([1, 1]), [[[1, 1], [2, 1], [2, 2]], 7]);
  assertEquals(board.group([2, 1]), [[[1, 1], [2, 1], [2, 2]], 7]);
  assertEquals(board.group([2, 2]), [[[1, 1], [2, 1], [2, 2]], 7]);
});

Deno.test("playMove", () => {
  for (let x = 0; x < 9; x += 1) {
    for (let y = 0; y < 9; y += 1) {
      for (const color of [Color.Black, Color.White]) {
        const board = new Board(9);
        const kills = board.playMove([[x, y], color]);
        assertEquals(kills, []);
        assertEquals(board.stones[board.toPos([x, y])], color);
      }
    }
  }
});

Deno.test("playMove pass", () => {
  const board = new Board(9);
  const kills = board.playMove([null, Color.Black]);
  assertEquals(kills, []);
});

Deno.test("playMove out of bounds", () => {
  const board = new Board(9);
  assertThrows(
    () => {
      board.playMove([[-1, 0], Color.Black]);
    },
    OutOfBoundsError,
  );
  assertThrows(
    () => {
      board.playMove([[10, 0], Color.Black]);
    },
    OutOfBoundsError,
  );
  assertThrows(
    () => {
      board.playMove([[0, -1], Color.Black]);
    },
    OutOfBoundsError,
  );
  assertThrows(
    () => {
      board.playMove([[0, 10], Color.Black]);
    },
    OutOfBoundsError,
  );
});

Deno.test("playMove on top of", () => {
  const board = new Board(9);
  board.playMove([[1, 1], Color.Black]);
  assertThrows(
    () => {
      board.playMove([[1, 1], Color.Black]);
    },
    SpaceOccupiedError,
  );
  assertThrows(
    () => {
      board.playMove([[1, 1], Color.White]);
    },
    SpaceOccupiedError,
  );
});

Deno.test("playMove kill one stone", () => {
  const board = new Board(9);
  board.playMove([[1, 0], Color.Black]);
  board.playMove([[0, 0], Color.White]);
  const kills = board.playMove([[0, 1], Color.Black]);
  assertEquals(kills, [[0, 0]]);
  assertEquals(board.stones[board.toPos([0, 0])], null);
});

Deno.test("playMove kill group", () => {
  const board = new Board(9);
  board.playMove([[1, 0], Color.Black]);
  board.playMove([[0, 1], Color.White]);
  board.playMove([[1, 1], Color.Black]);
  board.playMove([[0, 0], Color.White]);
  const kills = board.playMove([[0, 2], Color.Black]);
  assertEquals(kills, [[0, 0], [0, 1]]);
  assertEquals(board.stones[board.toPos([0, 0])], null);
  assertEquals(board.stones[board.toPos([0, 1])], null);
});

Deno.test("playMove suicide", () => {
  const board = new Board(9);
  board.playMove([[0, 1], Color.Black]);
  board.playMove([[1, 0], Color.Black]);
  assertThrows(
    () => {
      board.playMove([[0, 0], Color.White]);
    },
    SuicideError,
  );
});

Deno.test("playMove kill is not suicide", () => {
  const board = new Board(9);
  board.playMove([[1, 0], Color.Black]);
  board.playMove([[0, 1], Color.Black]);
  board.playMove([[1, 1], Color.White]);
  board.playMove([[2, 0], Color.White]);
  const kills = board.playMove([[0, 0], Color.White]);
  assertEquals(kills, [[1, 0]]);
  assertEquals(board.stones[board.toPos([1, 0])], null);
  assertEquals(board.stones[board.toPos([0, 0])], Color.White);
});
