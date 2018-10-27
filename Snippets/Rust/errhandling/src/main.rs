use std::io;
use std::io::Read;
use std::fs::File;
use std::io::ErrorKind;

fn main() {
  let f = match File::open("hello.txt") {
    Ok(file) => file,
    Err(ref error) if error.kind() == ErrorKind::NotFound => {
      match File::create("hello.txt") {
        Ok(cf) => cf,
        Err(e) => panic!("Error creating file hello.txt: {:?}", e),
      }
    }
    Err(error) => panic!("Error opening hello.txt: {:?}", error),
  };

  let f = File::open("hello2.txt").expect("Cannot open hello2.txt");

  println!("data: {}", read_from_file("hello3.txt").unwrap());

  let filename = String::from("hello4.txt");

  println!("data: {}", read_from_file(&filename).unwrap());
}

fn read_from_file(filename: &str) -> Result<String, io::Error> {
  let mut data = String::new();
  File::open(filename)?.read_to_string(&mut data)?;
  Ok(data)
}
