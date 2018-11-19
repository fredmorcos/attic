#[macro_use]
extern crate criterion;
extern crate fastwalkdir;

use fastwalkdir::Dir;
use criterion::Criterion;

fn info_new_home(c: &mut Criterion) {
  c.bench_function("home_dir", |b| b.iter(
    || Dir::new_from_str("/home/fred", vec!["/home/fred/.steam"])
      .expect("Error with home_dir")));
}

fn info_new_root(c: &mut Criterion) {
  c.bench_function("root_dir", |b| b.iter(
    || Dir::new_from_str(
      "/",
      vec!["/dev", "/root", "/proc", "/tmp",
           "/var", "/lost+found", "/sys", "/run",
           "/etc/ipsec.d/", "/etc/NetworkManager",
           "/etc/swanctl", "/etc/pacman.d/gnupg",
           "/etc/audisp", "/etc/sudoers.d",
           "/etc/libvirt/secrets", "/etc/wireguard",
           "/home/lost+found", "/home/fred/.steam"])
      .expect("Error with root_dir")));
}

criterion_group! {
  name = benches;
  config = Criterion::default().sample_size(3);
  targets = info_new_root, info_new_home
}

criterion_main!(benches);
