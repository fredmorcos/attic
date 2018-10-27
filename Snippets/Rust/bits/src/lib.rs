// use std::ops::{Shr, BitOr};
// use std::mem::size_of;

// pub trait Bits: Sized + Copy {
//   fn size() -> usize {
//     size_of::<Self>() * 8
//   }

//   fn get(self, index: usize) -> bool;
//   fn set(self, index: usize) -> Self;
// }

// impl<T> Bits for T
// where T: Sized + Copy + Shr<usize, Output = u8> + BitOr<Self, Output = Self>
// {
//   fn get(self, index: usize) -> bool {
//     if index > Self::size() {
//       panic!("Index is larger than bit-width({})", Self::size());
//     }

//     ((self >> index) & 1) == 1
//   }

//   fn set(self, index: usize) -> Self {
//     if index > Self::size() {
//       panic!("Index is larger than bit-width({})", Self::size());
//     }

//     self | (1 << index)
//   }
// }

// #[cfg(test)]
// mod tests {
//   #[test]
//   fn it_works() {
//     assert_eq!(2 + 2, 4);
//   }
// }

pub mod bits;
