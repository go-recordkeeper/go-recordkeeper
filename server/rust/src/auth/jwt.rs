use jsonwebtoken::{decode, encode, Algorithm, DecodingKey, EncodingKey, Header, Validation};
use serde::{Deserialize, Serialize};
use std::env;

#[derive(Debug, Serialize, Deserialize)]
struct Claims {
    sub: String,
    iat: u64,
    exp: u64,
    iss: String,
    aud: String,
}

impl Claims {
    fn new(id: i32) -> Claims {
        let now = jsonwebtoken::get_current_timestamp();
        Claims {
            sub: id.to_string(),
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

pub fn encode_jwt(id: i32) -> Result<String, jsonwebtoken::errors::Error> {
    let claims = Claims::new(id);
    encode(
        &Header::default(),
        &claims,
        &EncodingKey::from_secret(secret().as_ref()),
    )
}

pub fn decode_jwt(jwt: &str) -> Result<i32, jsonwebtoken::errors::Error> {
    decode::<Claims>(
        jwt,
        &DecodingKey::from_secret(secret().as_ref()),
        &Validation::new(Algorithm::HS256),
    )
    .map(|data| data.claims.sub.parse().unwrap())
}
