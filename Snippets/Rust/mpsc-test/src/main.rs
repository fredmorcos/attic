use std::thread;
use std::sync::mpsc::channel;
use std::io::Write;
use std::time::Duration;

fn main() {
  let (tx, rx) = channel();

  thread::spawn(move || {
    loop {
      match rx.recv() {
        Ok(msg) => println!("Received message '{}'", msg),
        Err(err) => {
          eprintln!("Receive Error: {}, closing...", err);
          break;
        },
      }
    }
  });

  loop {
    let mut msg = String::new();

    print!("Input: ");
    let _ = std::io::stdout().flush();

    match std::io::stdin().read_line(&mut msg) {
      Ok(0) => {
        println!("Got empty input, closing...");
        break;
      },
      Ok(msg_len) => {
        println!("Got input of {} bytes", msg_len);

        let msg = match msg.split_terminator(char::from(0xA)).nth(0) {
          Some(msg) => String::from(msg),
          None => {
            eprintln!("Got invalid input, retry...");
            continue;
          },
        };

        println!("Got input '{}' of {} characters", msg, msg.len());

        match tx.send(msg) {
          Ok(()) => {},
          Err(err) => {
            eprintln!("Send Error: {}, closing...", err);
            break;
          },
        }
      },
      Err(err) => {
        eprintln!("End of input stream: {}, closing...", err);
        break;
      }
    }

    thread::sleep(Duration::from_millis(500));
  }
}
