use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;
use crate::db::{query, query_one};

use super::go::Color;

pub async fn pass(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
) -> Result<impl IntoResponse, (StatusCode, &'static str)> {
    let handicap: i32 = query_one(
        &client,
        "SELECT handicap FROM record_record WHERE owner_id=$1 AND id=$2",
        &[&user_id, &record_id],
    )
    .await?
    .get("handicap");
    let moves: Vec<Color> = query(
        &client,
        "SELECT color FROM record_move WHERE record_id=$1 ORDER BY move ASC",
        &[&record_id],
    )
    .await?
    .iter()
    .map(|row| row.get("color"))
    .map(Color::new)
    .collect();
    let next_color = if moves.is_empty() || moves.len() < handicap.try_into().unwrap() {
        // First move is always black
        // If we are still in the handicap phase, next move is black
        "B"
    } else if let Some(Color::White) = moves.last() {
        // If the last move was white, the next move is black
        "B"
    } else {
        "W"
    };
    let move_number: i32 = (1 + moves.len()).try_into().unwrap();
    query(
        &client,
        "INSERT INTO record_move (record_id, position, color, move) VALUES ($1, $2, $3, $4)",
        &[&record_id, &None::<Option<i32>>, &next_color, &move_number],
    )
    .await?;
    Ok(StatusCode::CREATED)
}
