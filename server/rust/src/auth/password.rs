use base64::{engine::general_purpose, Engine as _};
use pbkdf2::pbkdf2_hmac;
use rand::{thread_rng, Rng};
use sha2::Sha256;

pub fn generate_salt() -> Vec<u8> {
    let mut salt = [0u8; 16];
    let mut rng = thread_rng();
    let whitelist = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    (0..16).for_each(|i| {
        let index = rng.gen_range(0..whitelist.len());
        salt[i] = whitelist[index];
    });
    salt.to_vec()
}

pub fn hash_password(salt: &[u8], password: &str) -> String {
    let n = 390000;
    let mut key = [0u8; 32];
    pbkdf2_hmac::<Sha256>(password.as_bytes(), salt, n, &mut key);
    let reference_hash = format!(
        "pbkdf2_sha256$390000${}${}",
        std::str::from_utf8(salt).expect("it to work"),
        general_purpose::STANDARD.encode(key),
    );
    reference_hash
}
