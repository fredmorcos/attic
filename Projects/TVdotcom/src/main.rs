#![warn(clippy::all)]

use futures::StreamExt;
use log::{debug, error, info};
use serde::Deserialize;
use std::collections::HashMap as Map;
use std::error::Error;
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;
use std::time::Duration;
use structopt::StructOpt;
use tokio::sync::Mutex;
use warp::{ws::WebSocket, Filter};

#[derive(Debug, Deserialize)]
enum Gender {
    Male,
    Female,
    TransMale,
    TransFemale,
}

#[derive(Debug, Deserialize)]
enum Age {
    Age23,
    Age24,
    Age25,
    Age26,
    Age27,
    Age28,
    Age29,
    Age30,
    Age31,
    Age32,
    Age33,
    Age34,
    Age35,
    Age36,
    Age37,
    Age38,
    Age39,
    Age40,
    Age41,
    Age42,
    Age43,
    Age44,
    Age45,
    Age46,
    Age47,
    Age48,
    Age49,
    Age50,
}

impl From<Age> for u8 {
    fn from(a: Age) -> Self {
        match a {
            Age::Age23 => 23,
            Age::Age24 => 24,
            Age::Age25 => 25,
            Age::Age26 => 26,
            Age::Age27 => 27,
            Age::Age28 => 28,
            Age::Age29 => 29,
            Age::Age30 => 30,
            Age::Age31 => 31,
            Age::Age32 => 32,
            Age::Age33 => 33,
            Age::Age34 => 34,
            Age::Age35 => 35,
            Age::Age36 => 36,
            Age::Age37 => 37,
            Age::Age38 => 38,
            Age::Age39 => 39,
            Age::Age40 => 40,
            Age::Age41 => 41,
            Age::Age42 => 42,
            Age::Age43 => 43,
            Age::Age44 => 44,
            Age::Age45 => 45,
            Age::Age46 => 46,
            Age::Age47 => 47,
            Age::Age48 => 48,
            Age::Age49 => 49,
            Age::Age50 => 50,
        }
    }
}

#[derive(Debug, Deserialize)]
struct Query {
    age: Age,
    gender: Gender,
    search_genders: Vec<Gender>,
    search_age: Age,
    search_sexual: bool,
}

struct Info {
    query: Query,
    reports: u8,
    reported: Vec<IpAddr>,
}

enum Status {
    NotMatched { info: Info },
    Matched { info: Info, connected_to: IpAddr },
}

type Users = Arc<Mutex<Map<IpAddr, Status>>>;

#[derive(Debug, StructOpt)]
struct TvDotCom {
    #[structopt(short, long, parse(from_occurrences))]
    verbose: u8,
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
    let opt = TvDotCom::from_args();

    let log_level = match opt.verbose {
        0 => log::LevelFilter::Warn,
        1 => log::LevelFilter::Info,
        2 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };
    env_logger::Builder::new()
        .filter_level(log_level)
        .try_init()?;

    let index_page = {
        let css_normalize = include_str!("../frontend/normalize.min.css");
        let css_skeleton = include_str!("../frontend/skeleton.min.css");
        let js = include_str!("../frontend/codez.min.js");

        Arc::new(
            include_str!("../frontend/index.min.html")
                .replace(
                    "$$$style$$$",
                    &format!("<style>{} {}</style>", css_normalize, css_skeleton),
                )
                .replace(
                    "$$$script$$$",
                    &format!(r#"<script type="text/javascript">{}</script>"#, js),
                ),
        )
    };

    let users = Arc::new(Mutex::new(Map::new()));

    let index = warp::path::end()
        .and(warp::any().map(move || index_page.clone()))
        .map(|page: Arc<String>| Box::new(warp::reply::html(page.to_string())));
    let ws = warp::path("ws")
        .and(warp::ws())
        .and(warp::filters::addr::remote())
        .and(warp::any().map(move || users.clone()))
        .and_then(handle_ws);

    warp::serve(index.or(ws)).run(([0, 0, 0, 0], 8000)).await;

    Ok(())
}

async fn handle_ws(
    ws: warp::ws::Ws,
    remote: Option<SocketAddr>,
    users: Users,
) -> Result<Box<dyn warp::Reply>, warp::Rejection> {
    if let Some(remote) = remote {
        info!("Got a websocket connection from {}", remote);
        Ok(Box::new(ws.on_upgrade(move |s: WebSocket| {
            user_conn(s, remote, users)
        })))
    } else {
        error!("Got a websocket connection without a remote");
        Ok(Box::new(warp::http::StatusCode::FORBIDDEN))
    }
}

async fn user_conn(ws: WebSocket, remote: SocketAddr, users: Users) {
    info!("Got a connection from {}", remote);

    let (_user_ws_tx, mut user_ws_rx) = ws.split();

    let query = match tokio::time::timeout(Duration::from_secs(10), user_ws_rx.next()).await {
        Ok(Some(query)) => query,
        Ok(None) => return,
        Err(e) => {
            error!("Waiting for query from {} timed out: {}", remote, e);
            return;
        }
    };

    let query = match query {
        Ok(query) => query,
        Err(e) => {
            error!("Error reading query: {}", e);
            return;
        }
    };

    if !query.is_text() {
        error!("Query message is not text");
        return;
    }

    let query = match query.to_str() {
        Ok(query) => query,
        Err(_) => {
            error!("Cannot convert query message to string ({:?})", query);
            return;
        }
    };

    let query: Query = match serde_json::from_str(&query) {
        Ok(query) => query,
        Err(e) => {
            error!("Invalid query ({:?}): {}", query, e);
            return;
        }
    };

    let ip = remote.ip();

    info!("Query from {} = {:?}", ip, query);

    {
        let mut users = users.lock().await;
        if let Some(_) = users.get(&ip) {
            error!("User {} already in DB", remote);
            return;
        } else {
            users.insert(
                ip,
                Status::NotMatched {
                    info: Info {
                        query,
                        reports: 0,
                        reported: Vec::new(),
                    },
                },
            );
        }
    }

    while let Some(msg) = user_ws_rx.next().await {
        match msg {
            Ok(msg) => {
                if msg.is_close() {
                    info!("User {} closed connection", ip);
                    disconnect_user(ip, users.clone()).await;
                    return;
                } else {
                    error!("Not accepting data from user {} at this point", ip);
                    disconnect_user(ip, users.clone()).await;
                    return;
                }
            }
            Err(e) => {
                error!("User {} error: {}", ip, e);
                disconnect_user(ip, users).await;
                return;
            }
        }
    }

    // while let Some(result) = user_ws_rx.next().await {
    //     let msg = match result {
    //         Ok(msg) => msg,
    //         Err(e) => {
    //             error!("Websocket error: {}", e);
    //             break;
    //         }
    //     };
    //     if !msg.is_text() {
    //         error!("Message is not text, invalid");
    //         break;
    //     }
    //     let msg = match msg.to_str() {
    //         Ok(msg) => msg,
    //         Err(_) => {
    //             error!("Cannot convert message to string ({:?})", msg);
    //             break;
    //         }
    //     };
    //     let query: Query = match serde_json::from_str(&msg) {
    //         Ok(query) => query,
    //         Err(e) => {
    //             error!("Invalid query ({:?}): {}", msg, e);
    //             break;
    //         }
    //     };
    //     info!("Query from {} = {:?}", remote.ip(), query);
    // }
}

async fn disconnect_user(ip: IpAddr, users: Users) {
    let mut users = users.lock().await;
    if let None = users.remove(&ip) {
        error!("Bad state, user {} was not in the DB", ip);
        return;
    }
    debug!("DB now has {} users", users.len());
}
