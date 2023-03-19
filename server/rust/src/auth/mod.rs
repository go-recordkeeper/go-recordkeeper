use axum::{
    body::Body,
    routing::{get, post},
    Router,
};
use std::sync::Arc;
use tokio_postgres::Client;

mod get_current_user;
mod login;
mod register;

/// Define all the routes
pub fn register_routes(router: Router<Arc<Client>, Body>) -> Router<Arc<Client>, Body> {
    router
        .route("/api/login/", post(login::login))
        .route("/api/register/", post(register::register))
        .route("/api/user/", get(get_current_user::get_current_user))
}
