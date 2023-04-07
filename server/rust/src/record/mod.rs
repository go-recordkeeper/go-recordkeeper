use axum::{
    body::Body,
    routing::{delete, get, post, put},
    Router,
};
use std::sync::Arc;
use tokio_postgres::Client;

// Go board helper
mod go;

// Endpoints
mod create;
mod delete;
mod get;
mod list;
mod pass;
mod play;
mod undo;
mod update;

pub fn register_routes(router: Router<Arc<Client>, Body>) -> Router<Arc<Client>, Body> {
    router
        .route("/api/records/", post(create::create))
        .route("/api/records/", get(list::list))
        .route("/api/records/:record/", get(get::get))
        .route("/api/records/:record/", put(update::update))
        .route("/api/records/:record/", delete(delete::delete))
        .route("/api/records/:record/play/", post(play::play))
        .route("/api/records/:record/pass/", post(pass::pass))
        .route("/api/records/:record/undo/", post(undo::undo))
}
