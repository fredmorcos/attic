use bytes::buf::Buf;
use futures::future::{self, Future};
use futures::stream::Stream;
use log::{error, info, warn};
use reqwest::r#async::Client;
use select::document::Document;
use select::predicate::Name;
use std::collections::HashMap;
use std::string::FromUtf8Error;
use std::sync::{Arc, Mutex};
use structopt::StructOpt;
use tokio::runtime::Runtime;
use url::Url;

type ReqErr = reqwest::Error;
type FetchRes = Result<String, FromUtf8Error>;

fn fetch(url: Url, client: Arc<Client>) -> impl Future<Item = FetchRes, Error = ReqErr> {
    client.get(url).send().and_then(|res| {
        res.into_body()
            .concat2()
            .map(|body| Ok(String::from_utf8(body.collect())?))
    })
}

fn get_elements<'i, 'a: 'i>(
    document: &'a Document,
    name: &'i str,
    attr_name: &'i str,
) -> impl Iterator<Item = &'a str> + 'i {
    document
        .find(Name(name))
        .filter_map(move |n| n.attr(attr_name))
}

fn concat(url: &Url, link: &str) -> Result<Option<Url>, url::ParseError> {
    let mut joined = url.join(link)?;
    let url_host = url.host();
    let joined_host = joined.host();

    if joined_host != url_host {
        return Ok(None);
    }

    normalize(&mut joined);

    Ok(Some(joined))
}

fn normalize(url: &mut Url) {
    url.set_fragment(None);
    url.set_query(None);
}

fn get_links<'d>(document: &'d Document, url: &'d Url) -> impl Iterator<Item = Url> + 'd {
    get_elements(&document, "a", "href").filter_map(move |l| match concat(url, l) {
        Ok(Some(link)) => Some(link),
        Ok(None) => None,
        Err(e) => {
            warn!("Cannot normalize URL `{} + {}`: {}", url, l, e);
            None
        }
    })
}

type DB = HashMap<Url, bool>;

fn crawl<'a>(
    url: Url,
    client: Arc<Client>,
) -> impl Future<Item = Vec<Url>, Error = Url> + 'a + Send {
    let url_clone = url.clone();
    let url_clone_fetch = url.clone();

    fetch(url_clone_fetch, client.clone())
        .map_err(move |e| {
            warn!("Error fetching page {}: {}", url_clone, e);
            url_clone
        })
        .and_then(move |contents| {
            info!("Fetched {}", url);

            match contents {
                Ok(contents) => {
                    let document = Document::from(contents.as_str());
                    future::ok(get_links(&document, &url).collect())
                }
                Err(e) => {
                    warn!("Error getting page contents for {}: {}", url, e);
                    future::err(url)
                }
            }
        })
        .from_err()
}

#[derive(StructOpt)]
struct Args {
    #[structopt(short = "-u")]
    url: String,
}

fn main_loop(mut rt: Runtime, urls: Vec<Url>, client: Arc<Client>, db: &mut DB) -> Result<(), ()> {
    if urls.is_empty() {
        return Ok(());
    }

    let futs = urls.into_iter().map(|l| crawl(l, client.clone()));
    let mut futs = tokio::prelude::stream::futures_unordered(futs);
    let mut new_urls = vec![];

    match rt.block_on(futs.collect()) {
        Ok(links) => {
            for link in links.into_iter().flatten() {
                db.entry(link.clone()).or_insert(true);
                futs.push(crawl(link, client.clone()));
            }
            // new_urls.extend(links.into_iter().flatten().collect::<Vec<Url>>());
        }
        Err(bad_url) => {
            error!("Error running stream element for {}", bad_url);
            db.insert(bad_url, false);
            return Err(());
        }
    }

    main_loop(rt, new_urls, client, db)
}

fn main() -> Result<(), ()> {
    env_logger::init();

    let args = Args::from_args();
    let client = Arc::new(Client::new());
    let mut url = Url::parse(&args.url).map_err(|e| {
        error!("Cannot parse URL: {}", e);
    })?;

    normalize(&mut url);

    let url_clone = url.clone();
    let mut db = DB::new();
    db.insert(url_clone, true);

    let rt = Runtime::new().map_err(|e| error!("Error creating async runtime: {}", e))?;

    main_loop(rt, vec![url], client, &mut db)?;

    info!("DB = {:?}", db);

    Ok(())
}
