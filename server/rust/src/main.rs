use std::sync::Arc;

use axum::http::{header, Method};
use axum::Router;
use tower_http::cors::{Any, CorsLayer};

mod auth;
mod db;
mod record;

#[tokio::main]
async fn main() {
    let client = Arc::new(db::connect().await.unwrap());

    let app = Router::new();
    let app = auth::register_routes(app).with_state(client.clone());
    let app = record::register_routes(app).with_state(client.clone());
    let app = if std::env::var("GOBAN_DEVELOPMENT").is_ok() {
        app.route_layer(
            CorsLayer::new()
                .allow_methods([Method::GET, Method::OPTIONS, Method::POST, Method::PUT])
                .allow_origin(Any)
                .allow_headers([header::CONTENT_TYPE, header::AUTHORIZATION])
                .expose_headers([header::CONTENT_DISPOSITION]),
        )
    } else {
        app
    };

    println!("Starting server...");
    axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
