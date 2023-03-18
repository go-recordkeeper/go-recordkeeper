use std::env;
use tokio_postgres::{tls::NoTlsStream, Client, Config, Error, NoTls};

pub async fn connect() -> Result<Client, Error> {
    let mut config = Config::new();
    config
        .user(&env::var("POSTGRES_USER").unwrap())
        .password(&env::var("POSTGRES_PASSWORD").unwrap())
        .dbname(&env::var("POSTGRES_NAME").unwrap())
        .host(&env::var("POSTGRES_HOST").unwrap());

    let (client, connection) = config.connect(NoTls).await?;

    // The connection is what communicates asynchronously with the database
    // Kick it off to it's own task
    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });

    Ok(client)
}
