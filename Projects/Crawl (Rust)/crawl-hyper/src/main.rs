#![warn(clippy::all)]

use futures::future;
use hyper::rt::{Future, Stream};
use hyper::service::service_fn;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use std::sync::{Arc, RwLock};

enum State {
    Idle,
    Busy,
    Finished,
}

fn service(
    req: Request<Body>,
    state: Arc<RwLock<State>>,
) -> impl Future<Item = Response<Body>, Error = hyper::Error> + Send {
    let mut res = Response::default();

    match (req.method(), req.uri().path()) {
        (&Method::POST, "/") => {
            match state.write() {
                Result::Ok(mut state) => match *state {
                    State::Busy => {
                        *res.status_mut() = StatusCode::PROCESSING;
                        return future::ok(res);
                    }

                    State::Idle | State::Finished => {
                        *state = State::Busy;
                    }
                },

                Result::Err(e) => {
                    *res.body_mut() = Body::from(format!("Error reading state: {}", e));
                    *res.status_mut() = StatusCode::CONFLICT;
                    return future::ok(res);
                }
            };
        }

        (&Method::GET, "/urls") => {}

        (&Method::GET, "/count") => {}

        _ => {
            *res.status_mut() = StatusCode::NOT_FOUND;
        }
    }

    future::ok(res)
}

fn main() {
    let addr = ([127, 0, 0, 1], 3000).into();
    let server = Server::bind(&addr)
        .serve(|| {
            let state = Arc::new(RwLock::new(State::Idle));
            service_fn(move |req| service(req, state.clone()))
        })
        .map_err(|e| eprintln!("Server error: {}", e));

    println!("Listening on {}", addr);

    hyper::rt::run(server);
}
