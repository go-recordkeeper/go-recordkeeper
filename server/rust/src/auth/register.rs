use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::{error::SqlState, Client};

use crate::auth::password::{generate_salt, hash_password};

use super::password::format_password_hash;

#[derive(Serialize, Deserialize)]
struct RegisterRequest {
    username: String,
    email: String,
    password: String,
}

#[derive(Serialize, Deserialize)]
struct RegisterResponse {
    id: i32,
    username: String,
    email: String,
}

pub async fn register(State(client): State<Arc<Client>>, body: String) -> impl IntoResponse {
    let RegisterRequest {
        username,
        email,
        password,
    } = serde_json::from_str(&body).unwrap();
    if !validator::validate_email(&email) {
        return Err((StatusCode::BAD_REQUEST, "Invalid email."));
    }
    let salt = generate_salt();
    let password_hash = hash_password(&salt, &password);
    let formatted_hash = format_password_hash(&salt, &password_hash);
    let now: chrono::DateTime<chrono::Utc> = chrono::Utc::now();
    let result = client.query_one("INSERT INTO auth_user (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active) VALUES ($1::TEXT, $2::TEXT, $3::TEXT, $4::TIMESTAMPTZ, $4::TIMESTAMPTZ, '', '', false, false, true) RETURNING id", &[&username, &email, &formatted_hash, &now]).await;
    if let Ok(new_user) = result {
        let id: i32 = new_user.get(0);
        Ok((
            StatusCode::CREATED,
            Json(
                RegisterResponse {
                    id,
                    username: username.to_string(),
                    email: email.to_string(),
                }, // StatusCode::NO_CONTENT,
            ),
        ))
    } else {
        if let Err(err) = result {
            if let Some(db_err) = err.as_db_error() {
                if db_err.code() == &SqlState::UNIQUE_VIOLATION {
                    return Err((
                        StatusCode::BAD_REQUEST,
                        "A user with that username already exists.",
                    ));
                }
            }
        }
        Err((StatusCode::INTERNAL_SERVER_ERROR, "DISASTER"))
    }
}
