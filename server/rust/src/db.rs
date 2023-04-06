use axum::{http::StatusCode, response::IntoResponse};
use std::env;
use tokio_postgres::{
    error::SqlState, types::ToSql, Client, Config, Error, NoTls, Row, ToStatement,
};

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

pub async fn query_one<T>(
    client: &Client,
    statement: &T,
    params: &[&(dyn ToSql + Sync)],
) -> Result<Row, (StatusCode, &'static str)>
where
    T: ?Sized + ToStatement,
{
    client.query_one(statement, params).await.map_err(|err| {
        if err.code() == Some(&SqlState::NO_DATA_FOUND) {
            (StatusCode::NOT_FOUND, "")
        } else {
            eprintln!("{}", err);
            (StatusCode::INTERNAL_SERVER_ERROR, "Something went wrong")
        }
    })
}

pub async fn query<T>(
    client: &Client,
    statement: &T,
    params: &[&(dyn ToSql + Sync)],
) -> Result<Vec<Row>, (StatusCode, &'static str)>
where
    T: ?Sized + ToStatement,
{
    client.query(statement, params).await.map_err(|err| {
        eprintln!("{}", err);
        (StatusCode::INTERNAL_SERVER_ERROR, "Something went wrong")
    })
}

#[macro_export]
macro_rules! derow {
    ($row:expr, {$($varname:ident : $vartype:ty),*}) => {
        (
            $(
                $row.get::<&str, $vartype>(stringify!($varname), ),
            )+
        )
    };
}
