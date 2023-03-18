use axum::{extract::State, http::StatusCode};
use base64::{engine::general_purpose, Engine as _};
use pbkdf2::pbkdf2_hmac;
use rand::{thread_rng, Rng};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::sync::Arc;
use tokio_postgres::Client;

#[derive(Serialize, Deserialize)]
struct RegisterRequest {
    username: String,
    email: String,
    password: String,
}

#[derive(Serialize, Deserialize)]
struct RegisterResponse {
    id: u32,
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

async fn hash_password(password: &str) -> String {
    let salt = generate_salt();
    let n = 390000;
    let mut key = [0u8; 64];
    pbkdf2_hmac::<Sha256>(password.as_bytes(), &salt, n, &mut key);
    let reference_hash = format!(
        "pbkdf2_sha256$390000${}${}",
        general_purpose::STANDARD_NO_PAD.encode(salt),
        hex::encode(key),
    );
    reference_hash
}

pub async fn register(
    State(client): State<Arc<Client>>,
    body: String,
) -> Result<String, StatusCode> {
    println!("Registering");
    let RegisterRequest {
        username,
        email,
        password,
    } = serde_json::from_str(&body).unwrap();
    let password_hash = hash_password(&password);
    let users = client
        .query("SELECT username, password FROM auth_user;", &[])
        .await
        .unwrap();
    println!("{:?}", users);
    let username: &str = users[0].get(0);
    let password: &str = users[0].get(1);
    println!("{}:{}", username, password);
    Ok(serde_json::to_string(&RegisterResponse {
        id: 666,
        username: username.to_string(),
        email: email.to_string(),
    })
    .unwrap())
}
