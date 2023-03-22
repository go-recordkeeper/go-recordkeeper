use axum::{
    async_trait,
    body::Body,
    extract::{FromRequestParts, State},
    http::{header::AUTHORIZATION, request::Parts, HeaderValue, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use jsonwebtoken::{Algorithm, DecodingKey, Validation};
use serde::{Deserialize, Serialize};
use std::env;
use std::sync::Arc;
use tokio_postgres::{error::SqlState, Client};

#[derive(Serialize, Deserialize)]
struct GetResponse {
    id: i32,
    username: String,
    email: String,
}

pub struct UserId(i32);

#[derive(Debug, Serialize, Deserialize)]
struct Claims {
    sub: i32,
    iat: u64,
    exp: u64,
    iss: String,
    aud: String,
}

fn secret() -> String {
    if env::var("GOBAN_DEVELOPMENT").is_ok() {
        "django-insecure-(@ppnpk$wx_z%2^#^0sext&+%b58=%e^!_u_*yd2p#d2&9)9cj".into()
    } else {
        env::var("GOBAN_SECRET_KEY").expect("GOBAN_SECRET_KEY not set")
    }
}

#[async_trait]
impl<S> FromRequestParts<S> for UserId
where
    S: Send + Sync,
{
    type Rejection = (StatusCode, &'static str);

    async fn from_request_parts(parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        parts
            .headers
            .get(AUTHORIZATION)
            .map(HeaderValue::to_str)
            .and_then(Result::ok)
            .and_then(|auth| auth.strip_prefix("Bearer "))
            .map(|jwt| {
                jsonwebtoken::decode::<Claims>(
                    jwt,
                    &DecodingKey::from_secret(secret().as_ref()),
                    &Validation::new(Algorithm::HS256),
                )
                .unwrap()
            })
            .map_or(
                Err((StatusCode::FORBIDDEN, "Failed to authenticate")),
                |token| Ok(UserId(token.claims.sub)),
            )
    }
}

pub async fn get_current_user(
    State(client): State<Arc<Client>>,
    UserId(user_id): UserId,
) -> impl IntoResponse {
    println!("gettin current user");
    println!("{}", user_id);
    let result = client
        .query_one(
            "SELECT username, email, is_active FROM auth_user WHERE id=$1",
            &[&user_id],
        )
        .await;
    if let Ok(user) = result {
        let username = user.get("username");
        let email = user.get("email");
        Ok(Json(GetResponse {
            id: user_id,
            username,
            email,
        }))
    } else {
        println!("woe {:?}", result);
        Err((StatusCode::FORBIDDEN, "not allowed"))
    }
}
