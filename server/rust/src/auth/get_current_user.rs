use axum::{
    async_trait,
    extract::{FromRequestParts, State},
    http::{header::AUTHORIZATION, request::Parts, HeaderValue, StatusCode},
    response::IntoResponse,
    Json,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio_postgres::Client;

use crate::auth::jwt::decode_jwt;

#[derive(Serialize, Deserialize)]
struct GetResponse {
    id: i32,
    username: String,
    email: String,
}

pub struct UserId(i32);

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
            .map(decode_jwt)
            .map_or(
                Err((StatusCode::FORBIDDEN, "Failed to authenticate")),
                |user_id| Ok(UserId(user_id.unwrap())),
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
