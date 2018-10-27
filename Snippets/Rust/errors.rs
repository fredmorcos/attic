// Create the Error, ErrorKind, ResultExt, and Result types
error_chain!{}

#[macro_export] macro_rules! chain {
  ($call:expr, $fmt:expr) => {
    $call.chain_err(|| $fmt)?
  };
  ($call:expr, $fmt:expr, $($arg:tt)+) => {
    $call.chain_err(|| format!($fmt, $($arg)+))?
  };
}

pub struct Ws {
  name: Option<String>,
  pub warnings: Vec<String>,
}

impl Ws {
  pub fn new(name: Option<String>) -> Ws {
    Ws { name: name, warnings: Vec::new() }
  }

  pub fn push_msg(&mut self, msg: &str) {
    if let Some(ref name) = self.name {
      self.warnings.push(format!("{}: {}", name, msg));
    } else {
      self.warnings.push(format!("{}", msg));
    }
  }

  pub fn append(&mut self, other: Ws) {
    for msg in other.warnings {
      self.push_msg(&msg);
    }
  }
}

#[macro_export] macro_rules! warn {
  ($w:ident, $fmt:expr) => {
    $w.push_msg($fmt);
  };
  ($w:ident, $fmt:expr, $($arg:tt)+) => {
    $w.push_msg(&format!($fmt, $($arg)+));
  };
}

#[macro_export] macro_rules! ewarn {
  ($w:ident, $call:expr, $fmt:expr) => {
    $w.append($call.chain_err(|| $fmt)?);
  };
  ($w:ident, $call:expr, $fmt:expr, $($arg:tt)+) => {
    $w.append($call.chain_err(|| format!($fmt, $($arg)+))?);
  };
}
