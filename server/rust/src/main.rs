use std::sync::Arc;

use axum::Router;

mod auth;
mod db;
mod record;

#[tokio::main]
async fn main() {
    let client = Arc::new(db::connect().await.unwrap());

    let app = Router::new();
    let app = auth::register_routes(app).with_state(client.clone());
    let app = record::register_routes(app).with_state(client.clone());

    println!("Starting server...");
    axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
