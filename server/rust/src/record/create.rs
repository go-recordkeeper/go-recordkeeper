use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;

#[derive(Serialize, Deserialize)]
struct CreateRequest {
    board_size: u32,
    name: Option<String>,
    black_player: Option<String>,
    white_player: Option<String>,
    comment: Option<String>,
    handicap: Option<i32>,
    komi: Option<f64>,
    ruleset: Option<String>,
}

#[derive(Serialize, Deserialize)]
struct CreateResponse {
    id: i64,
    owner: i32,
    board_size: u32,
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

pub async fn create(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    body: String,
) -> impl IntoResponse {
    let CreateRequest {
        board_size,
        name,
        black_player,
        white_player,
        comment,
        handicap,
        komi,
        ruleset,
    } = serde_json::from_str(&body).unwrap();
    if ![9, 13, 19].contains(&board_size) {
        return Err((StatusCode::BAD_REQUEST, "Invalid board size"));
    }
    let name = name.unwrap_or("".to_string());
    let black_player = black_player.unwrap_or("Black".to_string());
    let white_player = white_player.unwrap_or("White".to_string());
    let comment = comment.unwrap_or("".to_string());
    let handicap = handicap.unwrap_or(0);
    let komi = komi.unwrap_or(7.5);
    let ruleset = ruleset.unwrap_or("AGA".to_string());
    let winner = "U".to_string();
    let now: chrono::DateTime<chrono::Utc> = chrono::Utc::now();
    let result = client.query_one("INSERT INTO record_record (owner_id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11) RETURNING id", &[&user_id, &TryInto::<i32>::try_into(board_size).unwrap(), &name, &black_player, &white_player, &comment, &handicap, &komi, &ruleset, &winner, &now]).await;
    if let Ok(row) = result {
        let record_id = row.get("id");
        Ok((
            StatusCode::CREATED,
            Json(CreateResponse {
                id: record_id,
                owner: user_id,
                board_size,
                created: now,
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
    } else {
        println!("hmm disaster {:?}", result);
        Err((StatusCode::INTERNAL_SERVER_ERROR, "Error saving new record"))
    }
}
