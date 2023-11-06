use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::{
    auth::UserId,
    db::{query, query_one},
    record::go::{play_out_game, Color, Move, Pos},
};

#[derive(Serialize, Deserialize, PartialEq, Eq)]
struct Point {
    x: i32,
    y: i32,
}

impl PartialOrd for Point {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Point {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Serialize, Deserialize)]
struct MoveResponse {
    position: Option<Point>,
    color: String,
    captures: Vec<Point>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq)]
struct Stone {
    x: i32,
    y: i32,
    color: String,
}
impl PartialOrd for Stone {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Stone {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Serialize, Deserialize)]
struct GetResponse {
    id: i64,
    owner: i32,
    board_size: i32,
    created: DateTime<Utc>,
    name: String,
    black_player: String,
    white_player: String,
    comment: String,
    handicap: i32,
    komi: f64,
    ruleset: String,
    winner: String,
    moves: Vec<MoveResponse>,
    stones: Vec<Stone>,
}

pub async fn get(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
) -> Result<impl IntoResponse, (StatusCode, &'static str)> {
    let row = query_one(&client, "SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = $1 AND id = $2", &[&user_id, &record_id]).await?;
    let board_size = row.get("board_size");
    let created = row.get("created");
    let name = row.get("name");
    let black_player = row.get("black_player");
    let white_player = row.get("white_player");
    let comment = row.get("comment");
    let handicap = row.get("handicap");
    let komi = row.get("komi");
    let ruleset = row.get("ruleset");
    let winner = row.get("winner");
    let rows = query(
        &client,
        "SELECT position, color FROM record_move WHERE record_id=$1 ORDER BY move ASC",
        &[&record_id],
    )
    .await?;
    let moves: Vec<Move> = rows
        .iter()
        .map(|row| (row.get::<&str, Option<i32>>("position"), row.get("color")))
        .map(|(pos, color)| (pos.map(Pos::new), Color::new(color)))
        .collect();
    let (board, moves) = play_out_game(board_size, &moves).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            "Error playing moves in the DB",
        )
    })?;
    let moves = moves
        .iter()
        .map(|((pos, color), captures)| {
            let mut captures: Vec<Point> = captures
                .iter()
                .map(|p| p.to_coord(board_size))
                .map(|(x, y)| Point { x, y })
                .collect();
            captures.sort();
            MoveResponse {
                position: pos
                    .as_ref()
                    .map(|p| p.to_coord(board_size))
                    .map(|(x, y)| Point { x, y }),
                color: format!("{}", color),
                captures,
            }
        })
        .collect();
    let mut stones: Vec<Stone> = board
        .iter()
        .map(|(pos, color)| (pos.to_coord(board_size), format!("{}", color)))
        .map(|((x, y), color)| Stone { x, y, color })
        .collect();
    stones.sort();
    Ok(Json(GetResponse {
        id: record_id,
        owner: user_id,
        board_size,
        created,
        name,
        black_player,
        white_player,
        comment,
        handicap,
        komi,
        ruleset,
        winner,
        moves,
        stones,
    }))
}
