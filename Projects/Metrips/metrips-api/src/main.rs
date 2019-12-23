#![warn(clippy::all)]

pub mod models;
pub mod schema;

#[macro_use]
extern crate diesel;
#[macro_use]
extern crate diesel_migrations;

use actix_web::{self, get, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use derive_more::From;
use diesel::pg::PgConnection;
use diesel::prelude::*;
use diesel::r2d2::{Builder as ConnPoolBuilder, ConnectionManager as ConnManager};
use diesel::result::QueryResult;
use diesel_migrations::{embed_migrations, RunMigrationsError};
use dotenv::dotenv;
use flexi_logger as flog;
use listenfd::ListenFd;
use log::{error, info, warn};
use models::{NewTrip, NewUser, Trip, User};
use r2d2::{Error as ConnPoolError, Pool as ConnPool};
use std::{env, fmt, io};

embed_migrations!("migrations");

#[derive(Debug, From)]
enum HandlerError {
    ConnPool(ConnPoolError),
    DB(diesel::result::Error),
}

impl fmt::Display for HandlerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ConnPool(_) => write!(f, "Error: Connection pool"),
            Self::DB(_) => write!(f, "Error: database"),
        }
    }
}

impl actix_web::ResponseError for HandlerError {}

#[get("trips")]
fn trips(
    _req: HttpRequest,
    conn: web::Data<ConnPool<ConnManager<PgConnection>>>,
) -> Result<impl Responder, HandlerError> {
    use schema::trips::dsl::*;
    Ok(HttpResponse::Ok().json(trips.load::<Trip>(&conn.get()?)?))
}

#[get("profile")]
fn profile(
    _req: HttpRequest,
    conn: web::Data<ConnPool<ConnManager<PgConnection>>>,
) -> Result<impl Responder, HandlerError> {
    use schema::users::dsl::*;
    Ok(HttpResponse::Ok().json(users.load::<User>(&conn.get()?)?))
}

#[derive(Debug, From)]
enum AppError {
    Logger(flog::FlexiLoggerError),
    Actix(io::Error),
    Dotenv,
    Env(env::VarError),
    DBConnection(diesel::result::ConnectionError),
    DB(diesel::result::Error),
    Migrations(RunMigrationsError),
    ConnPool(ConnPoolError),
}

pub fn add_user<'a>(
    conn: &PgConnection,
    first_name: &'a str,
    last_name: &'a str,
    email: &'a str,
) -> QueryResult<User> {
    use schema::users;

    let new_user = NewUser {
        first_name,
        last_name,
        email,
    };

    diesel::insert_into(users::table)
        .values(&new_user)
        .get_result(conn)
}

pub fn add_trip<'a>(
    conn: &PgConnection,
    place: &'a str,
    description: &'a str,
    longitude: f64,
    latitude: f64,
) -> QueryResult<Trip> {
    use schema::trips;

    let new_trip = NewTrip {
        place,
        description,
        longitude,
        latitude,
    };

    diesel::insert_into(trips::table)
        .values(&new_trip)
        .get_result(conn)
}

fn main() -> Result<(), AppError> {
    // TODO Make logging more configurable using a specfile
    flog::Logger::with_str("info").start()?;

    // Load the .env file
    match dotenv() {
        Ok(path) => info!("Dotenv loaded path {}", path.display()),
        Err(err) => {
            error!("Dotenv loading error: {}", err);
            return Err(AppError::Dotenv);
        }
    }

    // Connect to the database
    let db_url = env::var("DATABASE_URL")?;
    let conn = PgConnection::establish(&db_url)?;
    info!("Established a connection to DB at {}", db_url);

    // Run database migrations
    info!("Running DB migrations...");
    embedded_migrations::run_with_output(&conn, &mut io::stderr())?;
    info!("Finished running DB migrations");

    // TODO Remove these dummy DB insertions
    let _dummy_del: User = diesel::delete(schema::users::table).get_result(&conn)?;
    let _dummy_user = add_user(&conn, "John", "Doe", "john.doe@mail.com")?;
    let _dummy_del: Trip = diesel::delete(schema::trips::table).get_result(&conn)?;
    let _dummy_trip = add_trip(&conn, "Nice trip", "It was a nice trip", 1.23, 2.23)?;
    let _dummy_trip = add_trip(&conn, "Went there", "It was great", 3.23, 4.23)?;
    let _dummy_trip = add_trip(&conn, "Wishlist place", "Maybe one day", 4.34, 6.23)?;
    let _dummy_trip = add_trip(&conn, "Romantic place", "Would be nice", 1.45, 5.23)?;
    warn!("Inserted dummy data into DB (See TODOs)");

    let conn_pool: ConnPool<ConnManager<PgConnection>> =
        ConnPoolBuilder::default().build(ConnManager::new(db_url))?;

    // TODO Load host and port from configuration
    let default_addr = "127.0.0.1:8080";

    let mut listenfd = ListenFd::from_env();
    let server = {
        // Register our handlers (ie, services)
        let server = HttpServer::new(move || {
            App::new()
                .data(conn_pool.clone())
                .service(trips)
                .service(profile)
        });

        if let Some(l) = listenfd.take_tcp_listener(0)? {
            server.listen(l)?
        } else {
            server.bind(default_addr)?
        }
    };

    // Run our app
    Ok(server.run()?)
}
