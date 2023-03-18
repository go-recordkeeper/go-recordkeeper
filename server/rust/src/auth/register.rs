use std::sync::Arc;

use axum::extract::State;
use base64::{engine::general_purpose, Engine as _};
use pbkdf2::pbkdf2_hmac;
use rand::{thread_rng, Rng};
use sha2::Sha256;
use tokio_postgres::Client;

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

pub async fn register(State(client): State<Arc<Client>>) {
    println!("Registering");
    let users = client
        .query("SELECT username, password FROM auth_user;", &[])
        .await
        .unwrap();
    println!("{:?}", users);
    let username: &str = users[0].get(0);
    let password: &str = users[0].get(1);
    println!("{}:{}", username, password);
}
