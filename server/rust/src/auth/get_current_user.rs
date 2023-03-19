use axum::{
    async_trait,
    extract::FromRequestParts,
    http::{header::AUTHORIZATION, request::Parts, HeaderValue, StatusCode},
};

pub struct AuthToken(String);

#[async_trait]
impl<S> FromRequestParts<S> for AuthToken
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
            .map_or(
                Err((StatusCode::BAD_REQUEST, "Failed to authenticate")),
                |token| Ok(AuthToken(token.to_string())),
            )
    }
}

pub async fn get_current_user(AuthToken(fooo): AuthToken) {
    println!("gettin current user");
    println!("{}", fooo);
}
