use crate::err::Error;
use log::info;
use std::io::Read;
use std::process;

pub fn generate() -> Result<(), Error> {
    'gen_loop: loop {
        let mut child = process::Command::new("pwgen")
            .args(&["-c", "-n", "-y", "-s", "-B", "-1", "34", "1"])
            .stdin(process::Stdio::null())
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped())
            .spawn()
            .map_err(Error::PwGenSpawn)?;

        let exit_status = child.wait().map_err(Error::PwGenWait)?;
        if !exit_status.success() {
            if let Some(code) = exit_status.code() {
                if let Some(mut err) = child.stderr {
                    let mut err_str = String::new();

                    if let Err(e) = err.read_to_string(&mut err_str) {
                        return Err(Error::PwGenStderrErr(code, e));
                    } else {
                        let err_str = err_str.trim().to_string();
                        if err_str.is_empty() {
                            return Err(Error::PwGenErr(code));
                        } else {
                            return Err(Error::PwGenErrMsg(code, err_str));
                        }
                    }
                } else {
                    return Err(Error::PwGenErr(code));
                }
            } else {
                return Err(Error::PwGenDied);
            }
        }

        if let Some(mut out) = child.stdout {
            let mut out_str = String::new();

            if let Err(e) = out.read_to_string(&mut out_str) {
                return Err(Error::PwGenStdoutErr(e));
            } else {
                let out_str = out_str.trim().to_string();

                if let Some(c) = out_str.chars().next() {
                    if c.is_ascii_punctuation() {
                        info!("Password ({}) starts with a symbol (skip)", out_str);
                        continue 'gen_loop;
                    } else {
                        if let Some(c) = out_str.chars().last() {
                            if c.is_ascii_punctuation() {
                                info!("Password ({}) ends with a symbol (skip)", out_str);
                                continue 'gen_loop;
                            } else {
                                println!("{}", out_str);
                                break 'gen_loop;
                            }
                        }
                    }
                } else {
                    return Err(Error::PwGenNoStdout);
                }
            }
        } else {
            return Err(Error::PwGenNoStdout);
        }
    }

    Ok(())
}
