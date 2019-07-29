#![warn(clippy::all)]

use futures::future::{self, Future};
use futures::stream::Stream;
use hyper::{
    rt::{self, Future as HyperFuture},
    service::service_fn,
    Body, Error as HyperError, Method, Request, Response, Server, StatusCode,
};
use std::{
    error::Error,
    fmt::{self, Display},
    net::{AddrParseError, SocketAddr},
    str,
    sync::{Arc, Mutex},
};

fn index(
    req: Request<Body>,
    name: Arc<Mutex<String>>,
) -> impl HyperFuture<Item = Response<Body>, Error = HyperError> + Send {
    let mut res = Response::new(Body::empty());

    match (req.method(), req.uri().path()) {
        (&Method::POST, "/") => match name.lock() {
            Ok(name) => {
                *name = req
                    .into_body()
                    .map_err(Error::from)
                    .concat2()
                    .and_then(|c| str::from_utf8(&c).map(str::to_owned).map_err(Error::from))
            }
            Err(e) => return future::err(String::new("Cannot access domain")),
        },
        _ => *res.status_mut() = StatusCode::NOT_FOUND,
    }

    future::ok(res)
}

#[derive(Debug)]
enum ScraperError {
    AddressParseError(AddrParseError),
}

impl Display for ScraperError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScraperError::AddressParseError(e) => write!(f, "Cannot parse network address: {}", e),
        }
    }
}

impl Error for ScraperError {}

fn main() -> Result<(), ScraperError> {
    let addr = "127.0.0.1:3000"
        .parse::<SocketAddr>()
        .map_err(ScraperError::AddressParseError)?;
    let dom = Arc::new(Mutex::new(String::new()));
    let service = || service_fn(|req| index(req, dom.clone()));
    let server = Server::bind(&addr)
        .serve(service)
        .map_err(|e| eprintln!("Server error: {}", e));
    println!("Listening on {}", addr);
    rt::run(server);
    Ok(())
}
