extern crate sync;
use sync::{Arc, Mutex};

fn main () {
  // let mut numbers = ~[1, 2, 3, 4];

  // let (tx, rx) = channel ();
  // numbers[0] = 4;
  // tx.send(numbers);

  // spawn (proc () {
  //   let mut numbers = rx.recv ();
  //   println! ("{}", numbers[0]);
  //   numbers[0] = 5;
  //   println! ("{}", numbers[0]);
  //   tx.send(numbers);
  // })

  // let numbers = ~[1,2,3];

  // for num in range(0, 3) {
  //   let (tx, rx) = channel();
  //   tx.send(numbers.clone());

  //   spawn(proc() {
  //     let numbers = rx.recv();
  //     println!("{}", numbers[num]);
  //   })
  // }

  // let numbers = ~[1,2,3];
  // let numbers = Arc::new(numbers);

  // for num in range(0, 3) {
  //   let (tx, rx) = channel();
  //   tx.send(numbers.clone());

  //   spawn(proc() {
  //     let numbers = rx.recv();
  //     println!("{:d}", numbers[num as uint]);
  //   })
  // }

  let numbers = ~[1,2,3];
  let numbers_lock = Arc::new(Mutex::new(numbers));

  for num in range(0, 3) {
    let (tx, rx) = channel();
    tx.send(numbers_lock.clone());

    spawn(proc() {
      let numbers_lock = rx.recv();

      let mut numbers = numbers_lock.lock();
      numbers[num as uint] += 1;

      println!("{}", numbers[num as uint]);
    })
  }
}
