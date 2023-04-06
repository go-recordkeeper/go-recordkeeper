use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    Json,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;
use crate::db::{query, query_one};
use crate::derow;
use crate::record::go::{find_captures, Color, GoError, Move, Pos};

#[derive(Serialize, Deserialize)]
struct PlayRequest {
    x: i32,
    y: i32,
}

#[derive(Serialize, Deserialize)]
struct Stone {
    color: String,
    x: i32,
    y: i32,
}

#[derive(Serialize, Deserialize)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Serialize, Deserialize)]
struct PlayResponse {
    add: Vec<Stone>,
    remove: Vec<Point>,
}

pub async fn play(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
    body: String,
) -> impl IntoResponse {
    let PlayRequest { x, y } = serde_json::from_str(&body).unwrap();
    let record_row = query_one(
        &client,
        "SELECT board_size, handicap FROM record_record WHERE owner_id=$1 AND id=$2",
        &[&user_id, &record_id],
    )
    .await?;
    let (board_size, handicap) = derow!(record_row, {board_size: i32, handicap: i32});
    let moves: Vec<Move> = query(
        &client,
        "SELECT position, color FROM record_move WHERE record_id=$1 ORDER BY move ASC",
        &[&record_id],
    )
    .await?
    .iter()
    .map(|row| derow!(row, {position: Option<i32>, color: &str}))
    .map(|(pos, color)| (pos.map(Pos::new), Color::new(color)))
    .collect();
    let pos = Pos::from_coord(board_size, (x, y)).index();
    let color = if moves.is_empty() || moves.len() < handicap.try_into().unwrap() {
        // First move is always black
        // If we are still in the handicap phase, next move is black
        Color::Black
    } else if let Some((_, Color::White)) = moves.last() {
        // If the last move was white, the next move is black
        Color::Black
    } else {
        Color::White
    };
    let captures = match find_captures(board_size, &moves, &(Some(Pos::new(pos)), color)) {
        Err(GoError::OutOfBounds(_)) => {
            return Err((StatusCode::FORBIDDEN, "Out of bounds"));
        }
        Err(GoError::SpaceOccupied(_)) => {
            return Err((StatusCode::FORBIDDEN, "Already a stone there"));
        }
        Err(GoError::Suicide(_)) => {
            return Err((StatusCode::FORBIDDEN, "Move is suicidal"));
        }
        Ok(captures) => captures,
    };
    let move_number: i32 = (1 + moves.len()).try_into().unwrap();
    query(
        &client,
        "INSERT INTO record_move (record_id, position, color, move) VALUES ($1, $2, $3, $4)",
        &[&record_id, &pos, &format!("{}", color), &move_number],
    )
    .await?;
    Ok((
        StatusCode::CREATED,
        Json(PlayResponse {
            add: vec![Stone {
                x,
                y,
                color: color.to_string(),
            }],
            remove: captures
                .iter()
                .map(|pos| pos.to_coord(board_size))
                .map(|(x, y)| Point { x, y })
                .collect(),
        }),
    ))
}
