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
use crate::record::go::Pos;

use super::go::{identify_captures, Color, Move};

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
    let result = client
        .query_one(
            "SELECT board_size, handicap FROM record_record WHERE owner_id=$1 AND id=$2",
            &[&user_id, &record_id],
        )
        .await;
    if let Ok(row) = result {
        let board_size: i32 = row.get("board_size");
        let handicap: i32 = row.get("handicap");
        let result = client
            .query(
                "SELECT position, color FROM record_move WHERE record_id=$1 ORDER BY move ASC",
                &[&record_id],
            )
            .await;
        if let Ok(rows) = result {
            let moves: Vec<Move> = rows
                .iter()
                .map(|row| (row.get::<&str, Option<i32>>("position"), row.get("color")))
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
            let captures =
                identify_captures(board_size, &moves, &(Some(Pos::new(pos)), Color::White))
                    .unwrap(); // TODO errors
            let move_number: i32 = 666;
            let result = client.query("INSERT INTO record_move (record_id, position, color, move) VALUES ($1, $2, $3, $4)", &[&record_id, &pos, &format!("{}",color), &move_number]).await;
            if result.is_ok() {
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
            } else {
                println!("{:?}", result);
                Err((StatusCode::INTERNAL_SERVER_ERROR, "Error saving new move"))
            }
        } else {
            Err((StatusCode::INTERNAL_SERVER_ERROR, "Error fetching moves"))
        }
    } else {
        Err((StatusCode::INTERNAL_SERVER_ERROR, "Error fetching record"))
    }
}
