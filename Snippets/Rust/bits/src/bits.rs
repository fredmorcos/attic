pub fn get(v: u32, index: u8) -> bool {
  ((v >> index) & 1) == 1
}

pub fn set(v: u32, index: u8) -> u32 {
  v | (1 << index)
}

pub fn intersection(v1: u32, v2: u32) -> u32 {
  v1 & v2
}

pub fn union(v1: u32, v2: u32) -> u32 {
  v1 | v2
}

pub struct Iter {
  v: u32,
  index: u8,
  max: u8,
}

pub fn iter(v: u32, n: u8) -> Iter {
  Iter {
    v,
    index: 0,
    max: n,
  }
}

impl Iterator for Iter {
  type Item = u8;

  fn next(&mut self) -> Option<Self::Item> {
    if self.index == self.max {
      None
    } else {
      if get(self.v, self.index) {
        let res = Some(self.index);
        self.index += 1;
        return res;
      } else {
        self.index += 1;
        self.next()
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use bits;

  #[test]
  fn get() {
    let x = 4;
    assert_eq!(bits::get(x, 0), false);
    assert_eq!(bits::get(x, 1), false);
    assert_eq!(bits::get(x, 2), true);
    assert_eq!(bits::get(x, 3), false);
  }

  #[test]
  fn set() {
    let x = bits::set(0, 2);
    assert_eq!(bits::get(x, 0), false);
    assert_eq!(bits::get(x, 1), false);
    assert_eq!(bits::get(x, 2), true);
    assert_eq!(bits::get(x, 3), false);
  }

  #[test]
  fn intersection() {
    assert_eq!(bits::intersection(4, 4), 4);
    assert_eq!(bits::intersection(1, 2), 0);
    assert_eq!(bits::intersection(7, 4), 4);
  }

  #[test]
  fn iter() {
    for i in bits::iter(5, 4) {
      println!("Bit {} is true", i);
    }
  }
}
