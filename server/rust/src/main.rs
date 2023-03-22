use std::sync::Arc;

use axum::Router;

mod auth;
mod db;

#[tokio::main]
async fn main() {
    let client = db::connect().await.unwrap();

    let app = Router::new();
    let app = auth::register_routes(app).with_state(Arc::new(client));

    println!("Starting server...");
    axum::Server::bind(&"0.0.0.0:8000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}
