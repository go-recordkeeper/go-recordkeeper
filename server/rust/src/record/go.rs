use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Color {
    Black,
    White,
}

impl Color {
    pub fn new(color: &str) -> Color {
        match color {
            "B" => Color::Black,
            "W" => Color::White,
            _ => panic!(),
        }
    }
    fn invert(&self) -> Color {
        match &self {
            Color::Black => Color::White,
            Color::White => Color::Black,
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Color::Black => write!(f, "B"),
            Color::White => write!(f, "W"),
        }
    }
}

type Coord = (i32, i32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pos(i32);
impl Pos {
    pub fn new(position: i32) -> Pos {
        Pos(position)
    }
    pub fn index(&self) -> i32 {
        self.0
    }
    pub fn to_coord(&self, board_size: i32) -> Coord {
        let Pos(pos) = &self;
        (pos % board_size, pos / board_size)
    }
    pub fn from_coord(board_size: i32, (x, y): Coord) -> Pos {
        Pos(x + (board_size * y))
    }
    fn adjacents(&self, board_size: i32) -> Vec<Pos> {
        let (x, y) = self.to_coord(board_size);
        vec![(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
            .into_iter()
            .filter(|&(x, y)| 0 <= x && x < board_size && 0 <= y && y < board_size)
            .map(|coord| Pos::from_coord(board_size, coord))
            .collect()
    }
}

pub type Move = (Option<Pos>, Color);
pub type Group = HashSet<Pos>;
pub type Liberties = HashSet<Pos>;

pub type Board = HashMap<Pos, Color>;

#[derive(Debug, PartialEq, Eq)]
pub enum GoError {
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
    board_size: i32,
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

fn place_stone(board_size: i32, board: &mut Board, move_: &Move) -> Result<Group> {
    match move_ {
        (None, _) => Ok(HashSet::new()),
        (Some(pos), color) => {
            // Verify the position is on the board
            if pos.index() < 0 || pos.index() > board_size * board_size {
                return Err(GoError::OutOfBounds(pos.clone()));
            }
            // Verify there is no stone at the desired location
            if board.get(pos).is_some() {
                return Err(GoError::SpaceOccupied(pos.to_coord(board_size)));
            }
            // Place the new stone on the board
            // This needs to be undone if the move is not valid
            board.insert(pos.clone(), *color);
            // Identify adjacent groups of the opposite color
            let captured_stones: HashSet<Pos> = pos
                .adjacents(board_size)
                .iter()
                .filter_map(|adj| build_group(board_size, board, adj, &color.invert()))
                .filter(|(_, liberties)| liberties.is_empty())
                .flat_map(|(group, _)| group)
                .collect();
            // Remove them from the board
            for capture in &captured_stones {
                board.remove(capture);
            }
            // Verify that the group of the newly placed stone has at least one liberty
            if let Some((_, liberties)) = build_group(board_size, board, pos, color) {
                if liberties.is_empty() {
                    // The move was suicidal, so fix the board
                    board.remove(pos);
                    return Err(GoError::Suicide(pos.to_coord(board_size)));
                }
            }
            Ok(captured_stones)
        }
    }
}

pub fn find_captures(board_size: i32, moves: &[Move], _move: &Move) -> Result<Group> {
    let mut board = HashMap::new();
    for m in moves.iter() {
        place_stone(board_size, &mut board, m)?;
    }
    place_stone(board_size, &mut board, _move)
}

pub fn play_out_game(board_size: i32, moves: &[Move]) -> Result<(Board, Vec<(Move, Group)>)> {
    let mut board = HashMap::new();
    // I would use an iterator but I couldn't figure out how to have a mutable reference to the
    // board passed around
    let mut captures = vec![];
    for m in moves.iter() {
        captures.push((m.clone(), place_stone(board_size, &mut board, m)?));
    }
    Ok((board, captures))
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
                    board.insert(pos!($x, $y), color!($color));
                )?
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
        // Two horizontal white stones in a corner
        assert_eq!(
            build_group(
                19,
                &board![(W, 0, 0), (W, 1, 0)],
                &pos!(0, 0),
                &Color::White
            ),
            Some((group![(0, 0), (1, 0)], group![(2, 0), (0, 1), (1, 1)]))
        );
        // Two vertical black stones in the middle
        assert_eq!(
            build_group(
                19,
                &board![(B, 1, 1), (B, 1, 2)],
                &pos!(1, 2),
                &Color::Black
            ),
            Some((
                group![(1, 1), (1, 2)],
                group![(1, 0), (0, 1), (2, 1), (0, 2), (2, 2), (1, 3)]
            ))
        );
    }

    #[test]
    fn build_group_liberty_counting() {
        // A white stone in the corner next to a black stone
        assert_eq!(
            build_group(
                19,
                &board![(W, 0, 0), (B, 1, 0)],
                &pos!(0, 0),
                &Color::White
            ),
            Some((group![(0, 0)], group![(0, 1)]))
        );
        // A white stone in the corner surrounded by black stones
        assert_eq!(
            build_group(
                19,
                &board![(W, 0, 0), (B, 1, 0), (B, 0, 1)],
                &pos!(0, 0),
                &Color::White
            ),
            Some((group![(0, 0)], group![]))
        );
        // A white stone in the middle surrounded by black stones
        assert_eq!(
            build_group(
                19,
                &board![(W, 1, 1), (B, 1, 0), (B, 0, 1), (B, 2, 1), (B, 1, 2)],
                &pos!(1, 1),
                &Color::White
            ),
            Some((group![(1, 1)], group![]))
        );
    }

    #[test]
    fn place_stone_simple_moves() {
        let test_cases = vec![
            (board![], color!(B), pos!(0, 0)),
            (board![], color!(W), pos!(1, 1)),
            (board![(B, 0, 0)], color!(B), pos!(1, 0)),
            (board![(B, 0, 0)], color!(W), pos!(1, 0)),
            (board![(B, 0, 0)], color!(W), pos!(0, 1)),
            (board![(B, 0, 1)], color!(W), pos!(0, 0)),
        ];
        for (mut board, color, pos) in test_cases {
            // Playing these moves should not capture anything
            assert_eq!(
                place_stone(19, &mut board, &(Some(pos.clone()), color)),
                Ok(group![]),
            );
            // The board should now contain the new move
            assert_eq!(board.get(&pos), Some(&color));
        }
    }

    #[test]
    fn place_stone_capture() {
        let test_cases = vec![
            // Capture a corner stones
            (board![(B, 0, 0), (W, 1, 0)], pos!(0, 1), group![(0, 0)]),
            // Capture two corner stones
            (
                board![(B, 0, 0), (B, 1, 0), (W, 0, 1), (W, 1, 1)],
                pos!(2, 0),
                group![(0, 0), (1, 0)],
            ),
            // Capture a ko that would otherwise be suicidal
            (
                board![(B, 0, 0), (B, 2, 0), (B, 1, 1), (W, 3, 0), (W, 2, 1)],
                pos!(1, 0),
                group![(2, 0)],
            ),
        ];
        for (mut board, pos, captures) in test_cases {
            // Playing should result in a capture
            assert_eq!(
                place_stone(19, &mut board, &(Some(pos.clone()), color!(W))),
                Ok(captures.clone()),
            );
            // The board should now be missing the captured stones
            for capture in &captures {
                assert_eq!(board.get(capture), None);
            }
        }
    }

    #[test]
    fn place_stone_out_of_bounds() {
        assert_eq!(
            place_stone(19, &mut board![], &(Some(pos!(20, 20)), color!(W))),
            Err(GoError::OutOfBounds(pos!(20, 20)))
        );
    }

    #[test]
    fn place_stone_on_existing_stone() {
        assert_eq!(
            place_stone(19, &mut board![(B, 0, 0)], &(Some(pos!(0, 0)), color!(W))),
            Err(GoError::SpaceOccupied((0, 0)))
        );
        assert_eq!(
            place_stone(19, &mut board![(B, 0, 0)], &(Some(pos!(0, 0)), color!(B))),
            Err(GoError::SpaceOccupied((0, 0)))
        );
    }

    #[test]
    fn place_stone_suicidally() {
        // Two stones surrounding the corner
        let mut board = board![(B, 1, 0), (B, 0, 1)];
        assert_eq!(
            place_stone(19, &mut board, &(Some(pos!(0, 0)), color!(W))),
            Err(GoError::Suicide((0, 0)))
        );
        // Verify the board was not changed
        assert_eq!(board, board![(B, 1, 0), (B, 0, 1)])
    }

    #[test]
    fn place_stone_suicidally_in_a_squeeze() {
        // Two white stones surrounding the corner, surrounded by three black stones
        let mut board = board![(W, 1, 0), (W, 0, 1), (B, 2, 0), (B, 1, 1), (B, 0, 2)];
        assert_eq!(
            place_stone(19, &mut board, &(Some(pos!(0, 0)), color!(W))),
            Err(GoError::Suicide((0, 0)))
        );
        // Verify the board was not changed
        assert_eq!(
            board,
            board![(W, 1, 0), (W, 0, 1), (B, 2, 0), (B, 1, 1), (B, 0, 2)]
        );
    }
}
