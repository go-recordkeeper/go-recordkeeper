use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::jwt::encode_jwt;
use crate::auth::password::{hash_password, parse_formatted_hash};

#[derive(Serialize, Deserialize)]
struct LoginRequest {
    username: String,
    password: String,
}

pub async fn login(State(client): State<Arc<Client>>, body: String) -> impl IntoResponse {
    let LoginRequest { username, password } = serde_json::from_str(&body).unwrap();
    let result = client
        .query_one(
            "SELECT id, password FROM auth_user WHERE username=$1",
            &[&username],
        )
        .await;
    // Dummy values, these will all be replaced if the user exists
    let mut id = 0;
    let mut salt: Vec<u8> = [0; 16].to_vec();
    let mut hash: String = "not a real password hash".into();
    if let Ok(user) = result {
        id = user.get("id");
        let formatted_hash: String = user.get("password");
        (salt, hash) = parse_formatted_hash(&formatted_hash);
    }
    // This hash will always happen for any input, which prevents timing attacks
    let login_attempt = hash_password(&salt, &password);
    if login_attempt == hash {
        encode_jwt(id)
            .map(Json)
            .or(Err((StatusCode::UNAUTHORIZED, "sus")))
    } else {
        Err((StatusCode::UNAUTHORIZED, "sus"))
    }
}
