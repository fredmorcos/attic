# Metrips API

The `metrips` API offers an HTTP-based API and server that supports
the loading and storing of trip information for users.

## Building and running

Assuming the `Rust` language toolchain is installed, running the
application is as simple as executing the following inside the project
top-level directory:

`cargo run    # for a debug build`

and:

`cargo run --release   # for a release build`

## Running with auto-reload

Some binaries are necessary for auto-reload to work:

`cargo install systemfd cargo-watch`

and running the server:

`systemfd --no-pid -s http::8080 -- cargo watch -x run`
