use axum::{
    body::Body,
    routing::{get, post, put},
    Router,
};
use std::sync::Arc;
use tokio_postgres::Client;

// Go board helper
mod go;

// Endpoints
mod create;
mod get;
mod list;
mod pass;
mod play;
mod update;

pub fn register_routes(router: Router<Arc<Client>, Body>) -> Router<Arc<Client>, Body> {
    router
        .route("/api/records/", post(create::create))
        .route("/api/records/", get(list::list))
        .route("/api/records/:record/", get(get::get))
        .route("/api/records/:record/", put(update::update))
        .route("/api/records/:record/play/", post(play::play))
        .route("/api/records/:record/pass/", post(pass::pass))
}
