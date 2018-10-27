extern crate itertools;
// use itertools::{kmerge, merge};
use itertools::Itertools;

fn main() {
  // for elt in kmerge(vec![vec![0, 2, 4], vec![1, 3, 5], vec![6, 7]]) {
  //   println!("{}", elt);
  // }

  // for elt in merge(&[1, 2, 3], &[2, 3, 4]) {
  //   println!("{}", elt);
  // }

  // println!("---");

  let v = vec![vec![1,2,3,4,5,6,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![1,2,3,4,5,6,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![4,5,6,7,8,9,7,8,9,0],
               vec![1,2,3,4,5,6,7,8,9,0]];

  let mut t = 1;

  for x in &v {
    t *= x.len();
  }

  let mut t2 = 0;
  let mut e = 0;

  for res in v.iter().multi_cartesian_product() {
    // println!("{:?}", res);
    e += res.len();
    t2 += 1;
  }

  assert_eq!(t, t2);

  println!("{} == {}", t, t2);
  println!("{}", e);
}
