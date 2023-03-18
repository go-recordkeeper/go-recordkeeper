use std::sync::Arc;

use axum::{routing::get, Router};

mod auth;
mod db;

#[tokio::main]
async fn main() {
    let client = db::connect().await.unwrap();
    let users = client
        .query("SELECT username, password FROM auth_user;", &[])
        .await
        .unwrap();
    println!("{:?}", users);
    let username: &str = users[0].get(0);
    let password: &str = users[0].get(1);
    println!("{}:{}", username, password);

    // build our application with a single route
    let app = Router::new().route("/", get(|| async { "Hello, World!" }));
    let app = auth::register_routes(app).with_state(Arc::new(client));

    // run it with hyper on localhost:3000
    axum::Server::bind(&"0.0.0.0:3000".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

