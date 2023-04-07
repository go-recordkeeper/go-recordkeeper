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
struct UndoResponse {
    add: Vec<Stone>,
    remove: Vec<Point>,
}

pub async fn undo(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
) -> impl IntoResponse {
    let record_row = query_one(
        &client,
        "SELECT board_size FROM record_record WHERE owner_id=$1 AND id=$2",
        &[&user_id, &record_id],
    )
    .await?;
    let board_size = record_row.get("board_size");
    let mut moves: Vec<Move> = query(
        &client,
        "SELECT position, color FROM record_move WHERE record_id=$1 ORDER BY move ASC",
        &[&record_id],
    )
    .await?
    .iter()
    .map(|row| derow!(row, {position: Option<i32>, color: &str}))
    .map(|(pos, color)| (pos.map(Pos::new), Color::new(color)))
    .collect();
    if moves.is_empty() {
        return Err((StatusCode::FORBIDDEN, "No moves to undo"));
    }
    let move_to_undo = moves.pop().unwrap();
    // The most recent move needs to be removed from the board
    let removals = if let (Some(pos), _) = &move_to_undo {
        let (x, y) = pos.to_coord(board_size);
        vec![Point { x, y }]
    } else {
        vec![]
    };
    // Any stones that move captured need to be added back to the board
    // They will have the opposite color from the move being undone
    let addition_color = format!("{}", move_to_undo.1.invert().clone());
    let additions = match find_captures(board_size, &moves, &move_to_undo) {
        Err(GoError::OutOfBounds(_)) => {
            return Err((StatusCode::FORBIDDEN, "Out of bounds"));
        }
        Err(GoError::SpaceOccupied(_)) => {
            return Err((StatusCode::FORBIDDEN, "Already a stone there"));
        }
        Err(GoError::Suicide(_)) => {
            return Err((StatusCode::FORBIDDEN, "Move is suicidal"));
        }
        Ok(captures) => captures
            .iter()
            .map(|pos| pos.to_coord(board_size))
            .map(|(x, y)| Stone {
                x,
                y,
                color: addition_color.clone(),
            })
            .collect(),
    };
    let move_number: i32 = (1 + moves.len()).try_into().unwrap();
    query(
        &client,
        "DELETE FROM record_move WHERE record_id=$1 AND move=$2",
        &[&record_id, &move_number],
    )
    .await?;
    Ok((
        StatusCode::OK,
        Json(UndoResponse {
            add: additions,
            remove: removals,
        }),
    ))
}
