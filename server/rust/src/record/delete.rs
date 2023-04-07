use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::UserId;
use crate::db::{query, query_one};

pub async fn delete(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
    Path(record_id): Path<i64>,
) -> Result<impl IntoResponse, (StatusCode, &'static str)> {
    // Verify the record exists before we try to delete it
    query_one(
        &client,
        "SELECT id FROM record_record WHERE owner_id=$1 AND id=$2",
        &[&user_id, &record_id],
    )
    .await?;
    // Delete the moves first because Django didn't set up cascading deletes properly
    query(
        &client,
        "DELETE FROM record_move WHERE record_id=$1",
        &[&record_id],
    )
    .await?;
    query(
        &client,
        "DELETE FROM record_record WHERE owner_id=$1 AND id=$2",
        &[&user_id, &record_id],
    )
    .await?;
    Ok(StatusCode::NO_CONTENT)
}
