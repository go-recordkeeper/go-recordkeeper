use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Color {
    White,
    Black,
}

impl Color {
    fn invert(&self) -> Color {
        match self {
            &Color::Black => Color::White,
            &Color::White => Color::Black,
        }
    }
}

type Coord = (u32, u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Pos(u32);
impl Pos {
    fn new(position: u32) -> Pos {
        return Pos(position);
    }
    fn index(&self) -> u32 {
        self.0
    }
    fn to_coord(&self, board_size: u32) -> Coord {
        let Pos(pos) = &self;
        (pos % board_size, pos / board_size)
    }
    fn from_coord(board_size: u32, (x, y): Coord) -> Pos {
        Pos(x + (board_size * y))
    }
    fn to_coord_signed(&self, board_size: u32) -> (i32, i32) {
        let Pos(pos) = &self;
        (
            (pos % board_size).try_into().unwrap(),
            (pos / board_size).try_into().unwrap(),
        )
    }
    fn from_coord_signed(board_size: i32, (x, y): (i32, i32)) -> Pos {
        Pos((x + (board_size * y)).try_into().unwrap())
    }
    fn adjacents(&self, board_size: u32) -> Vec<Pos> {
        let (x, y) = self.to_coord_signed(board_size);
        let board_size: i32 = board_size.try_into().unwrap();
        vec![(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
            .into_iter()
            .filter(|&(x, y)| 0 <= x && x < board_size && 0 <= y && y < board_size)
            .map(|coord| Pos::from_coord_signed(board_size, coord))
            .collect()
    }
}

type Move = (Option<Pos>, Color);
type Group = HashSet<Pos>;
type Liberties = HashSet<Pos>;

type Board = HashMap<Pos, Color>;

#[derive(Debug)]
enum GoError {
    OutOfBounds(Pos),
    SpaceOccupied(Coord),
    Suicide(Coord),
}

impl fmt::Display for GoError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GoError::OutOfBounds(pos) => write!(f, "Out of bounds: {:?}", pos),
            GoError::SpaceOccupied(coord) => {
                write!(f, "Space already occupied: ({}, {})", coord.0, coord.1)
            }
            GoError::Suicide(coord) => write!(f, "Suicide attempt: ({}, {})", coord.0, coord.1),
        }
    }
}

impl Error for GoError {}

type Result<T> = std::result::Result<T, GoError>;

fn build_group(
    board_size: u32,
    board: &Board,
    pos: &Pos,
    color: &Color,
) -> Option<(Group, Liberties)> {
    let mut group = HashSet::new();
    let mut liberties = HashSet::new();
    let group_color = board.get(pos);
    // If there is no stone at the given position, return an empty group
    if group_color.is_none() || group_color.unwrap() != color {
        return None;
    }
    let group_color = group_color.unwrap();

    // Keep the queue as a vec and as a set.
    // This allows constant time iteration and contains()
    let mut queue = vec![];
    queue.push(pos.clone());
    group.insert(pos.clone());

    while let Some(pos) = queue.pop() {
        let stone = board.get(&pos);

        if let Some(color) = stone {
            if color == group_color {
                pos.adjacents(board_size).iter().for_each(|adj| {
                    if !group.contains(adj) {
                        queue.push(adj.clone());
                    }
                });
                group.insert(pos);
            }
        } else {
            liberties.insert(pos);
        }
    }
    Some((group, liberties))
}

fn place_stone(board_size: u32, board: &mut Board, move_: &Move) -> Result<Group> {
    match move_ {
        (None, _) => Ok(HashSet::new()),
        (Some(pos), color) => {
            // Verify the position is on the board
            if pos.index() > board_size * board_size {
                return Err(GoError::OutOfBounds(pos.clone()));
            }
            // Verify there is no stone at the desired location
            if board.get(pos).is_some() {
                return Err(GoError::SpaceOccupied(pos.to_coord(board_size)));
            }
            // Place the new stone on the board
            board.insert(pos.clone(), *color);
            // Identify adjacent groups of the opposite color
            let captured_stones: HashSet<Pos> = pos
                .adjacents(board_size)
                .iter()
                .filter_map(|adj| build_group(board_size, board, adj, &color.invert()))
                .flat_map(|(group, _)| group)
                .collect();
            // Remove them from the board
            for capture in &captured_stones {
                board.remove(capture);
            }
            // Verify that the group of the newly placed stone has at least one liberty
            if let Some((_, liberties)) = build_group(board_size, board, pos, color) {
                if liberties.is_empty() {
                    return Err(GoError::Suicide(pos.to_coord(board_size)));
                }
            }
            Ok(captured_stones)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper macros for making it less verbose to write tests
    macro_rules! pos {
        ($pos:expr) => {
            Pos::new($pos)
        };
        ($x:expr, $y:expr) => {
            // Just assume a board size of 19
            Pos::from_coord(19, ($x, $y))
        };
    }

    macro_rules! color {
        (W) => {
            Color::White
        };
        (B) => {
            Color::Black
        };
    }

    macro_rules! board {
        () => {
            HashMap::new()
        };
        ($(($color:tt, $x:expr, $y:expr)),+) => {
            {
                let mut board = board!();
                $(
                    place_stone(19, &mut board, &(Some(pos!($x, $y)), color!($color))).unwrap();
                )?
                // TODO play the moves
                board
            }
        };
    }

    macro_rules! group {
        () => {
            HashSet::new()
        };
        ($(($x:expr, $y:expr)),+) => {{
            let mut group: Group = group!();
            $(
                group.insert(pos!($x, $y));
            )?
            group
        }};
    }

    #[test]
    fn from_coord() {
        assert_eq!(Pos::from_coord(3, (0, 0)), Pos(0));
        assert_eq!(Pos::from_coord(3, (1, 0)), Pos(1));
        assert_eq!(Pos::from_coord(3, (2, 0)), Pos(2));
        assert_eq!(Pos::from_coord(3, (0, 1)), Pos(3));
        assert_eq!(Pos::from_coord(3, (1, 1)), Pos(4));
        assert_eq!(Pos::from_coord(3, (2, 1)), Pos(5));
        assert_eq!(Pos::from_coord(3, (0, 2)), Pos(6));
        assert_eq!(Pos::from_coord(3, (1, 2)), Pos(7));
        assert_eq!(Pos::from_coord(3, (2, 2)), Pos(8));
    }

    #[test]
    fn build_group_empty() {
        // A group on an empty board is None
        assert_eq!(build_group(19, &board![], &pos!(0, 0), &Color::White), None);
        // A group on a unoccupied square is None
        assert_eq!(
            build_group(19, &board![(B, 1, 1)], &pos!(0, 0), &Color::White),
            None
        );
        // A group on an occupied square of the wrong color is None
        assert_eq!(
            build_group(19, &board![(B, 1, 1)], &pos!(1, 1), &Color::White),
            None
        );
    }

    #[test]
    fn build_group_one_stone() {
        // One white stone in a corner
        assert_eq!(
            build_group(19, &board![(W, 0, 0)], &pos!(0, 0), &Color::White),
            Some((group![(0, 0)], group![(1, 0), (0, 1)]))
        );
        // One stone with four liberties
        assert_eq!(
            build_group(19, &board![(B, 1, 1)], &pos!(1, 1), &Color::Black),
            Some((group![(1, 1)], group![(1, 0), (0, 1), (2, 1), (1, 2)]))
        );
    }

    #[test]
    fn build_group_two_stones() {
        // Two white stones in a corner
        &board![(W, 0, 0), (W, 1, 0)];
        // assert_eq!(
        //     build_group(
        //         19,
        //         &board![(W, 0, 0), (W, 1, 0)],
        //         &pos!(0, 0),
        //         &Color::White
        //     ),
        //     Some((group![(0, 0), (1, 0)], group![(2, 0), (0, 1), (1, 1)]))
        // );
        // assert_eq!(
        //     build_group(19, &board![(B, 1, 1)], &pos!(1, 1), &Color::Black),
        //     Some((group![(1, 1)], group![(1, 0), (0, 1), (2, 1), (1, 2)]))
        // );
    }
}
