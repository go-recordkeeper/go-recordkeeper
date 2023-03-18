use axum::{
    async_trait,
    extract::FromRequestParts,
    http::{header::AUTHORIZATION, request::Parts, HeaderValue, StatusCode},
    routing::{get, post},
    Router,
};

struct ExtractAuthorizationBearer(String);

#[async_trait]
impl<S> FromRequestParts<S> for ExtractAuthorizationBearer
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
                |token| Ok(ExtractAuthorizationBearer(token.to_string())),
            )
    }
}

async fn login() {
    println!("loggin in");
}

async fn register() {
    println!("Registering");
}

async fn get_current_user(ExtractAuthorizationBearer(fooo): ExtractAuthorizationBearer) {
    println!("gettin current user");
    println!("{}", fooo);
}

/// Define all the routes
pub fn register_routes(router: Router) -> Router {
    router
        .route("/api/login/", post(login))
        .route("/api/register/", post(register))
        .route("/api/user/", get(get_current_user))
}
