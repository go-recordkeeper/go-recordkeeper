use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use base64::{engine::general_purpose, Engine as _};
use pbkdf2::pbkdf2_hmac;
use rand::{thread_rng, Rng};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::sync::Arc;
use tokio_postgres::{error::SqlState, Client};

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

fn generate_salt() -> [u8; 16] {
    let mut salt = [0u8; 16];
    let mut rng = thread_rng();
    let whitelist = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    (0..16).for_each(|i| {
        let index = rng.gen_range(0..whitelist.len());
        salt[i] = whitelist[index];
    });
    salt
}

fn hash_password(password: &str) -> String {
    let salt = generate_salt();
    let n = 390000;
    let mut key = [0u8; 32];
    pbkdf2_hmac::<Sha256>(password.as_bytes(), &salt, n, &mut key);
    let reference_hash = format!(
        "pbkdf2_sha256$390000${}${}",
        general_purpose::STANDARD_NO_PAD.encode(salt),
        general_purpose::STANDARD_NO_PAD.encode(key),
    );
    reference_hash
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
    let password_hash = hash_password(&password);
    let now: chrono::DateTime<chrono::Utc> = chrono::Utc::now();
    let result = client.query_one("INSERT INTO auth_user (username, email, password, date_joined, last_login, first_name, last_name, is_superuser, is_staff, is_active) VALUES ($1::TEXT, $2::TEXT, $3::TEXT, $4::TIMESTAMPTZ, $4::TIMESTAMPTZ, '', '', false, false, true) RETURNING id", &[&username, &email, &password_hash, &now]).await;
    if let Ok(new_user) = result {
        let id: i32 = new_user.get(0);
        if id == 4 {}
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
