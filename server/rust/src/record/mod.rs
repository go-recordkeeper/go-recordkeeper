use axum::{
    body::Body,
    routing::{get, post},
    Router,
};
use std::sync::Arc;
use tokio_postgres::Client;

// Endpoints
mod create;
mod list;

pub fn register_routes(router: Router<Arc<Client>, Body>) -> Router<Arc<Client>, Body> {
    router
        .route("/api/records/", post(create::create))
        .route("/api/records/", get(list::list))
}
