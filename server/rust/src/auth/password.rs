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
    general_purpose::STANDARD.encode(key)
}

pub fn format_password_hash(salt: &[u8], hash: &str) -> String {
    format!(
        "pbkdf2_sha256$390000${}${}",
        std::str::from_utf8(salt).expect("it to work"),
        hash,
    )
}

pub fn parse_formatted_hash(formatted_hash: &str) -> (Vec<u8>, String) {
    let hash_re =
        regex::Regex::new(r"^pbkdf2_sha256\$390000\$([a-zA-Z0-9+/]+)\$([a-zA-Z0-9+/=]+)$")
            .expect("Invalid password hash in DB");
    let captures = hash_re.captures(formatted_hash).unwrap();
    let salt = captures
        .get(1)
        .unwrap()
        .as_str()
        .as_bytes()
        .try_into()
        .expect("Unprocessable salt string");
    let hash = captures.get(2).unwrap().as_str().into();
    (salt, hash)
}
