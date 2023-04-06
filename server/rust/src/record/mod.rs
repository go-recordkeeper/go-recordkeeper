use axum::{
    body::Body,
    routing::{get, post},
    Router,
};
use std::sync::Arc;
use tokio_postgres::Client;

// Go board helper
mod go;

// Endpoints
mod create;
mod list;
mod play;

pub fn register_routes(router: Router<Arc<Client>, Body>) -> Router<Arc<Client>, Body> {
    router
        .route("/api/records/", post(create::create))
        .route("/api/records/", get(list::list))
        .route("/api/records/:record/play/", post(play::play))
}
