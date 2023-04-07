use axum::{
    extract::{Path, State},
    http::{
        header::{CONTENT_DISPOSITION, CONTENT_TYPE},
        HeaderMap, StatusCode,
    },
};
use chrono::{DateTime, Utc};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;
use crate::db::{query, query_one};
use crate::derow;
use crate::record::go::Pos;

pub async fn download(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
) -> Result<(StatusCode, HeaderMap, String), (StatusCode, &'static str)> {
    let row = query_one(&client, "SELECT id, board_size, name, black_player, white_player, comment, handicap, komi, ruleset, winner, created FROM record_record WHERE owner_id = $1 AND id = $2", &[&user_id, &record_id]).await?;
    let board_size: i32 = row.get("board_size");
    let created: DateTime<Utc> = row.get("created");
    let name: String = row.get("name");
    let black_player: String = row.get("black_player");
    let white_player: String = row.get("white_player");
    let comment: String = row.get("comment");
    let handicap: i32 = row.get("handicap");
    let komi: f64 = row.get("komi");
    let ruleset: String = row.get("ruleset");
    let winner: String = row.get("winner");

    let comment = if !comment.is_empty() {
        format!("GC[{}]", comment)
    } else {
        "".to_string()
    };
    let name = if !name.is_empty() {
        format!("GN[{}]", name)
    } else {
        "".to_string()
    };
    let winner = match winner.as_ref() {
        "U" => "Void",
        "B" => "B+R",
        "W" => "W+R",
        _ => {
            return Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                "Error parsing the winner",
            ))
        }
    }
    .to_string();

    let rows = query(
        &client,
        "SELECT position, color FROM record_move WHERE record_id=$1 ORDER BY move ASC",
        &[&record_id],
    )
    .await?;
    let alphabet: Vec<&str> = vec![
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",
        "s", "t",
    ];
    let formatted_moves: String = rows
        .iter()
        .map(|row| derow!(row, {position: Option<i32>, color: &str}))
        .map(|(pos, color)| (pos.map(Pos::new), color))
        .map(|(pos, color)| (pos.map(|pos| pos.to_coord(board_size)), color))
        .map(|(pos, color)| {
            (
                match pos {
                    Some((x, y)) => format!(
                        "{}{}",
                        alphabet[TryInto::<usize>::try_into(x).unwrap()],
                        alphabet[TryInto::<usize>::try_into(y).unwrap()]
                    ),
                    None => "tt".to_string(),
                },
                color,
            )
        })
        .map(|(pos, color)| format!("{}[{}]", color, pos))
        .collect::<Vec<String>>()
        .join(";");
    let sgf_file = format!(
        "(;FF[4]CA[UTF-8]{}GM[1]{}HA[{}]KM[{}]PB[{}]PW[{}]RE[{}]SZ[{}];{})",
        comment,
        name,
        handicap,
        komi,
        black_player,
        white_player,
        winner,
        board_size,
        formatted_moves
    );
    let date_string = created.format("%Y_%m_%d");
    let filename = if !name.is_empty() {
        format!("{}_{}.sgf", name, date_string)
    } else {
        format!("{}_vs_{}_{}.sgf", black_player, white_player, date_string)
    };
    let mut headers = HeaderMap::new();
    headers.insert(CONTENT_TYPE, "application/x-go-sgf".parse().unwrap());
    headers.insert(
        CONTENT_DISPOSITION,
        format!("attachment; filename=\"{}\"", filename)
            .parse()
            .unwrap(),
    );
    Ok((StatusCode::OK, headers, sgf_file))
}
