FROM archlinux:latest
RUN pacman -Syu --noconfirm rustup gcc
RUN pacman -Sccc --noconfirm
RUN rustup install --profile minimal stable

WORKDIR /app
ADD . /app
RUN cargo build --release
RUN cp target/release/tvdotcom /tvdotcom
RUN rm -rf /app/*
RUN cp /tvdotcom /app/.
RUN rustup uninstall stable
RUN rm -rf ~/.cargo
RUN rm -rf ~/.rustup

EXPOSE 8000
CMD ["/app/tvdotcom", "-vvvv"]
