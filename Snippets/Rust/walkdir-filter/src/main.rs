#![warn(clippy::all)]

use log::{error, info, warn};
use rayon::prelude::*;
use std::path::Path;
use std::process;
use walkdir::WalkDir;

struct DirSpec<'r, 'i> {
    root: &'r Path,
    ignores: Vec<&'i Path>,
}

impl<'r, 'i> DirSpec<'r, 'i> {
    fn new(root: &'r Path, ignores: Vec<&'i Path>) -> Self {
        Self { root, ignores }
    }
}

// struct Dir<'r, 'i, 'f> {
//     spec: DirSpec<'r, 'i>,
//     files: Vec<&'f Path>,
// }

fn main() {
    env_logger::init();

    // rayon::ThreadPoolBuilder::new()
    //     .num_threads(256)
    //     .build_global()
    //     .unwrap();

    let dir_specs = &[
        DirSpec::new(
            Path::new("/boot"),
            [
                "intel-ucode.img",
                "vmlinuz-linux",
                "EFI",
                "initramfs-linux-fallback.img",
                "initramfs-linux.img",
            ]
            .into_iter()
            .map(|f| Path::new(*f))
            .collect(),
        ),
        DirSpec::new(
            Path::new("/etc"),
            [
                "ca-certificates",
                "netctl",
                "privoxy",
                "pam.d",
                "fonts",
                "strongswan.d",
                "xinetd.d",
                "lvm",
                "speech-dispatcher",
                "cups",
                "openldap",
                "xdg",
                "fwupd",
            ]
            .into_iter()
            .map(|f| Path::new(*f))
            .collect(),
        ),
    ];
    let _dir_specs = dir_specs
        // .into_par_iter()
        .into_iter()
        .filter(|p| {
            if !p.root.has_root() {
                warn!("Ignore {}: It does not start at root", p.root.display());
            }
            p.root.has_root()
        })
        .for_each(|dir_spec| {
            for entry in WalkDir::new(dir_spec.root) {
                let entry = match entry {
                    Ok(entry) => entry,
                    Err(err) => {
                        if let Some(path) = err.path() {
                            warn!("Ignore {}: {}", path.display(), err);
                        } else {
                            warn!("Ignore {}", err);
                        }
                        continue;
                    }
                };

                if entry.file_type().is_file() {
                    match process::Command::new("pacman")
                        .args(&["-Q", "-o", &entry.path().to_string_lossy()])
                        .output()
                    {
                        Ok(output) => {
                            if !output.status.success() {
                                warn!(
                                    "Could not find file owner for {} (process exited{})",
                                    entry.path().display(),
                                    if let Some(code) = output.status.code() {
                                        format!(" with exit code {}", code)
                                    } else {
                                        String::from("with unknown reason")
                                    }
                                )
                            } else {
                                match String::from_utf8(output.stdout) {
                                    Ok(stdout) => info!(
                                        "Collect {} -> {}",
                                        entry.path().display(),
                                        stdout.trim()
                                    ),
                                    Err(err) => warn!(
                                        "Collect {} (invalid output from process: {})",
                                        entry.path().display(),
                                        err
                                    ),
                                }
                            }
                        }
                        Err(err) => warn!(
                            "Error finding owner of file {}: {}",
                            entry.path().display(),
                            err
                        ),
                    }

                    // info!("Collect {}", entry.path().display());
                }
            }
        });
}
