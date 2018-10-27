extern crate bindgen;
extern crate pkg_config;

use std::env;
use std::path::PathBuf;

fn main() {
  pkg_config::probe_library("libcec").unwrap();

  let libcec_bindings = bindgen::Builder::default()
    .header("/usr/include/libcec/cecc.h")
    .rust_target(bindgen::RustTarget::Nightly)
    .generate_comments(false)
    .layout_tests(false)
    .generate()
    .expect("Unable to generate libCEC bindings");

  let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

  libcec_bindings.write_to_file(out_path.join("cec_bindings.rs"))
                 .expect("Couldn't write libCEC bindings");
}
