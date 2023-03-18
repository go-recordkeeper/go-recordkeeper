use tokio_postgres::{tls::NoTlsStream, Client, Config, Error, NoTls};

pub async fn connect() -> Result<Client, Error> {
    let mut config = Config::new();
    // TODO load these configurations from environment variables
    config
        .user("postgres")
        .password("postgres")
        .dbname("default")
        .host("localhost");

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
