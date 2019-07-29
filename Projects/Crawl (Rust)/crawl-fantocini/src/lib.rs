use fantoccini as fanto;
use futures::future::Future;
use std::error::Error;
use std::fmt::{self, Display};

const WEBDRIVER: &str = "localhost:4444";

struct Scraper {
    client: fanto::Client,
}

#[derive(Debug)]
enum ScraperError {
    ClientError(fanto::error::NewSessionError),
}

impl Display for ScraperError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScraperError::ClientError(e) => write!(f, "Cannot connect to webdriver: {}", e),
        }
    }
}

impl Error for ScraperError {}

impl Scraper {
    fn new(link: &str) -> Scraper {
        let client = tokio::run(
            fanto::Client::new(WEBDRIVER)
                .map_err(|e| panic!("Cannot connect to webdriver: {}", e))
                .and_then(|c| c.find(fanto::Locator::Css("a")))
                .map_err(|e| panic!("Webdriver command failed: {}", e)),
        );
        Scraper { client }
    }

    fn get_links(link: &str) -> Vec<String> {}
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
