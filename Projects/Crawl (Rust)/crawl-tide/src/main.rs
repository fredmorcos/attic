#![warn(clippy::all)]
#![feature(async_await)]

use std::sync::atomic::{self, AtomicBool};
use std::sync::{Arc, RwLock};
use std::thread;
use tide::error::ResultExt;
use tide::response::IntoResponse;

enum Crawler {
    Idle,
    Task(Arc<AtomicBool>),
}

impl Default for Crawler {
    fn default() -> Self {
        Crawler::Idle
    }
}

impl Crawler {
    fn start(&mut self, _domain: &str) -> bool {
        if let Crawler::Idle = self {
            let done = Arc::new(AtomicBool::new(false));
            let done_clone = done.clone();
            let _handle = thread::spawn(move || {
                // TODO Implement crawler here
                std::thread::sleep(std::time::Duration::from_secs(10));

                // TODO Figure out a way to return errors here
                done_clone.store(true, atomic::Ordering::SeqCst);
            });

            *self = Crawler::Task(done);
            return true;
        }

        false
    }
}

type State = RwLock<Crawler>;

async fn submit(mut context: tide::Context<State>) -> tide::EndpointResult<String> {
    let domain: String = context.body_string().await.client_err()?;
    {
        let mut crawler = context.app_data().write().map_err(|e| {
            format!("Error submitting domain {}: {}", domain, e)
                .with_status(tide::http::StatusCode::CONFLICT)
                .into_response()
        })?;

        if !crawler.start(&domain) {
            return Err(format!("Error starting task for domain {}", domain)
                .with_status(tide::http::StatusCode::CONFLICT)
                .into_response())?;
        }
    }
    Ok(format!("Submitted domain {}\n", domain))
}

async fn urls(mut context: tide::Context<State>) -> tide::EndpointResult<String> {
    {
        let crawler = context.app_data().read().map_err(|e| {
            format!("Error getting URLs, cannot read app data: {}", e)
                .with_status(tide::http::StatusCode::CONFLICT)
                .into_response()
        })?;

        if let Crawler::Task(done) = &*crawler {
            if done.load(atomic::Ordering::SeqCst) {
                // TODO Return list of URLs
                return Ok(String::from("Finished\n"));
            } else {
                return Err(String::from("Not finished, try again later\n")
                    .with_status(tide::http::StatusCode::CONFLICT)
                    .into_response())?;
            }
        } else {
            return Err(String::from("Nothing submitted\n")
                .with_status(tide::http::StatusCode::CONFLICT)
                .into_response())?;
        }
    }
}

async fn count(mut _context: tide::Context<State>) -> tide::EndpointResult<()> {
    unimplemented!()
}

fn main() -> Result<(), std::io::Error> {
    let mut app = tide::App::new(RwLock::new(Crawler::default()));
    app.at("/").post(submit);
    app.at("/urls").get(urls);
    app.at("/count").get(count);
    Ok(app.serve("127.0.0.1:8000")?)
}
