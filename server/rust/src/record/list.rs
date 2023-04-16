use axum::extract::Query;
use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;
use crate::db::{query, query_one};

#[derive(Serialize, Deserialize)]
struct ListRecordResponse {
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

#[derive(Serialize, Deserialize)]
struct ListResponse {
    count: i64,
    pages: i64,
    results: Vec<ListRecordResponse>,
}

#[derive(Deserialize)]
pub struct Pagination {
    page_size: Option<i64>,
    page: Option<i64>,
}

pub async fn list(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Query(pagination): Query<Pagination>,
) -> Result<impl IntoResponse, (StatusCode, &'static str)> {
    let page_size = pagination.page_size.unwrap_or(10);
    let page = pagination.page.unwrap_or(1);
    if page_size < 1 {
        return Err((StatusCode::NOT_FOUND, "Invalid page size"));
    }
    let row = query_one(
        &client,
        "SELECT COUNT(*) FROM record_record WHERE owner_id = $1",
        &[&user_id],
    )
    .await?;
    let count: i64 = row.get(0);
    let pages = count / page_size;
    let pages = if pages == 0 { 1 } else { pages };
    if page < 1 || page > pages {
        return Err((StatusCode::NOT_FOUND, "Invalid page number"));
    }
    let rows = query(&client,"SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = $1 ORDER BY CREATED DESC LIMIT $2 OFFSET $3", &[&user_id, &page_size, &((page-1) * page_size)]).await?;
    let results = rows
        .iter()
        .map(|row| ListRecordResponse {
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
        .collect();
    Ok(Json(ListResponse {
        count,
        pages,
        results,
    }))
}
