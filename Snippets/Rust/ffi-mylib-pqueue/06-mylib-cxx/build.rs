use std::path::Path;

fn main() {
    let lib = Path::new("src").join("lib.rs");
    let test = Path::new("tests").join("test_cxx.cc");

    cxx_build::bridge(lib.to_str().unwrap())
        .file(test.to_str().unwrap())
        .flag_if_supported("-std=c++17")
        .compile("test_cxx");

    println!("cargo:rerun-if-changed={}", lib.display());
    println!("cargo:rerun-if-changed={}", test.display());
}
