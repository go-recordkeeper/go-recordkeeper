use axum::extract::Path;
use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;
use crate::db::query_one;

#[derive(Serialize, Deserialize)]
struct UpdateRequest {
    name: String,
    black_player: String,
    white_player: String,
    comment: String,
    handicap: i32,
    komi: f64,
    ruleset: String,
    winner: String,
}

#[derive(Serialize, Deserialize)]
struct UpdateResponse {
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
}

pub async fn update(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
    body: String,
) -> Result<impl IntoResponse, (StatusCode, &'static str)> {
    let UpdateRequest {
        name,
        black_player,
        white_player,
        comment,
        handicap,
        komi,
        ruleset,
        winner,
    } = serde_json::from_str(&body).unwrap();
    let row = query_one(&client, "UPDATE record_record SET name=$3, black_player=$4, white_player=$5, comment=$6, handicap=$7, komi=$8, ruleset=$9, winner=$10 WHERE owner_id=$1 AND id=$2 RETURNING board_size, created", &[&user_id, &record_id, &name, &black_player, &white_player, &comment, &handicap, &komi, &ruleset, &winner]).await?;
    let board_size = row.get("board_size");
    let created = row.get("created");
    Ok((
        StatusCode::OK,
        Json(UpdateResponse {
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
        }),
    ))
}
