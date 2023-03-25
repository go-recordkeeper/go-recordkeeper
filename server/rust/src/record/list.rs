use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;

#[derive(Serialize, Deserialize)]
struct ListResponse {
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

pub async fn list(State(client): State<Arc<Client>>, UserId(user_id): UserId) -> impl IntoResponse {
    let result = client.query("SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = $1 ORDER BY CREATED DESC", &[&user_id]).await;
    if let Ok(rows) = result {
        Ok(Json(
            rows.iter()
                .map(|row| ListResponse {
                    id: row.get("id"),
                    owner: user_id,
                    board_size: row.get("board_size"),
                    created: row.get("created"),
                    name: row.get("name"),
                    black_player: row.get("black_player"),
                    white_player: row.get("white_player"),
                    comment: row.get("comment"),
                    handicap: row.get("handicap"),
                    komi: row.get("komi"),
                    ruleset: row.get("ruleset"),
                    winner: row.get("winner"),
                })
                .collect::<Vec<ListResponse>>(),
        ))
    } else {
        Err((StatusCode::INTERNAL_SERVER_ERROR, "Error saving new record"))
    }
}
