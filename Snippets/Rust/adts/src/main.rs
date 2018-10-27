use std::collections::HashMap;

#[derive(Debug)]
enum Cell {
  Int(i32),
  Float(f64),
  Text(String),
}

fn main() {
  let v = vec![1, 2, 3, 4, 5];
  let third: &i32 = &v[2];
  let third: Option<&i32> = v.get(2);

  let row = vec![
    Cell::Int(3),
    Cell::Float(43.4),
    Cell::Text(String::from("foobar")),
  ];

  for c in row {
    println!("{:?}", &c);
  }

  let text = "hello world wonderful world";
  let mut map: HashMap<&str, u32> = HashMap::new();

  for word in text.split_whitespace() {
    let count: &mut u32 = map.entry(&word).or_insert(0);
    *count += 1;
  }

  println!("{:?}", map);
}
