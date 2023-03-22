use axum::{
    async_trait,
    body::Body,
    extract::{FromRequestParts, State},
    http::{header::AUTHORIZATION, request::Parts, HeaderValue, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use base64::{engine::general_purpose, Engine as _};
use pbkdf2::pbkdf2_hmac;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::env;
use std::sync::Arc;
use tokio_postgres::{error::SqlState, Client};

#[derive(Serialize, Deserialize)]
struct LoginRequest {
    username: String,
    password: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Claims {
    sub: i32,
    iat: u64,
    exp: u64,
    iss: String,
    aud: String,
}

impl Claims {
    fn new(id: i32) -> Claims {
        let now = jsonwebtoken::get_current_timestamp();
        Claims {
            sub: id,
            iat: now,
            // Increment the timestamp by one day's worth of seconds
            exp: now + (24 * 60 * 60),
            iss: "go-recordkeeper".into(),
            aud: "go-recordkeeper".into(),
        }
    }
}

fn secret() -> String {
    if env::var("GOBAN_DEVELOPMENT").is_ok() {
        "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj".into()
    } else {
        env::var("GOBAN_SECRET_KEY").expect("GOBAN_SECRET_KEY not set")
    }
}

// TODO dedup
pub fn hash_password(salt: &[u8], password: &str) -> String {
    let n = 390000;
    let mut key = [0u8; 32];
    pbkdf2_hmac::<Sha256>(password.as_bytes(), salt, n, &mut key);
    general_purpose::STANDARD.encode(key)
}

pub async fn login(State(client): State<Arc<Client>>, body: String) -> impl IntoResponse {
    println!("loggin in");
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
        let password_hash: String = user.get("password");
        let hash_re =
            regex::Regex::new(r"^pbkdf2_sha256\$390000\$([a-zA-Z0-9+/]+)\$([a-zA-Z0-9+/=]+)$")
                .expect("Invalid password hash in DB");
        let captures = hash_re.captures(&password_hash).unwrap();
        salt = captures
            .get(1)
            .unwrap()
            .as_str()
            .as_bytes()
            .try_into()
            .expect("Unprocessable salt string");
        hash = captures.get(2).unwrap().as_str().into();
    }
    // This hash will always happen for any input, which prevents timing attacks
    let login_attempt = hash_password(&salt, &password);
    if login_attempt == hash {
        let claims = Claims::new(id);
        jsonwebtoken::encode(
            &jsonwebtoken::Header::default(),
            &claims,
            &jsonwebtoken::EncodingKey::from_secret(secret().as_ref()),
        )
        .map(Json)
        .or(Err((StatusCode::UNAUTHORIZED, "sus")))
    } else {
        Err((StatusCode::UNAUTHORIZED, "sus"))
    }
}
