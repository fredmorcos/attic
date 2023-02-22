FROM rust:latest

# Install mob.sh tool
RUN curl -sL install.mob.sh | sh

# Install some rustup components that are available by default.
RUN rustup component add rustfmt clippy rust-src rust-docs rust-analysis rls llvm-tools-preview
