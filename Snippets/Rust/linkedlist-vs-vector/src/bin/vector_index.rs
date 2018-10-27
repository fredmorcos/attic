extern crate ll_vs_vec;

use ll_vs_vec::extras;

fn main() {
  let vec = extras::create_vector(100_000_000);
  extras::traverse_vector_index(&vec);
}
