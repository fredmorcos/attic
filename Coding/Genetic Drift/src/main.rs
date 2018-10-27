#![feature(tool_lints)]
#![warn(clippy::all)]

fn oriented_pairs(perm: &[i32]) -> Vec<(i32, i32)> {
  let mut pairs: Vec<(i32, i32)> = Vec::new();

  for x in perm {
    if *x >= 0 {
      for neg_int in perm.iter().filter(|&&y| y < 0) {
        if
          ((neg_int.abs() - x).abs() == 1) &&
          !pairs.contains(&(*x, *neg_int)) && !pairs.contains(&(*neg_int, *x))
        {
          pairs.push((*x, *neg_int));
        }
      }
    } else if *x < 0 {
      for pos_int in perm.iter().filter(|&&y| y > 0) {
        if
          ((x.abs() - pos_int).abs() == 1) &&
          !pairs.contains(&(*x, *pos_int)) && !pairs.contains(&(*pos_int, *x))
        {
          pairs.push((*x, *pos_int));
        }
      }
    }
  }

  pairs.sort_unstable_by(|(x1, _), (x2, _)| x1.cmp(x2));
  pairs
}

fn oriented_pairs_with_indexes(perm: &[i32]) -> Vec<((i32, usize), (i32, usize))> {
  let mut pairs: Vec<((i32, usize), (i32, usize))> = Vec::new();

  for (i, x) in perm.iter().enumerate() {
    if *x >= 0 {
      for (j, neg_int) in perm.iter().enumerate().filter(|&(_, &y)| y < 0) {
        if
          ((neg_int.abs() - x).abs() == 1) &&
          !pairs.contains(&((*x, i), (*neg_int, j))) && !pairs.contains(&((*neg_int, j), (*x, i)))
        {
          pairs.push(((*x, i), (*neg_int, j)));
        }
      }
    } else if *x < 0 {
      for (j, pos_int) in perm.iter().enumerate().filter(|&(_, &y)| y > 0) {
        if
          ((x.abs() - pos_int).abs() == 1) &&
          !pairs.contains(&((*x, i), (*pos_int, j))) && !pairs.contains(&((*pos_int, j), (*x, i)))
        {
          pairs.push(((*x, i), (*pos_int, j)));
        }
      }
    }
  }

  pairs.sort_unstable_by(|(x1, _), (x2, _)| x1.cmp(x2));
  pairs
}

fn oriented_pair_inversion(perm: &[i32], xi: i32, i: usize, xj: i32, j: usize) -> Vec<i32> {
  let mut res: Vec<i32> = Vec::new();

  let diff = xi + xj;
  assert!(diff.abs() == 1);

  if diff == 1 {
    for index in i .. j {
      res.insert(0, -perm[index]);
    }

    for index in 0 .. i {
      res.insert(index, perm[index]);
    }

    for index in j .. perm.len() {
      res.push(perm[index]);
    }
  } else if diff == -1 {
    for index in i + 1 .. j + 1 {
      res.insert(0, -perm[index]);
    }

    for index in 0 .. i + 1 {
      res.insert(index, perm[index]);
    }

    for index in j + 1 .. perm.len() {
      res.push(perm[index]);
    }
  } else {
    unreachable!();
  }

  res
}

fn score(perm: &[i32], xi: i32, i: usize, xj: i32, j: usize) -> usize {
  oriented_pairs(&oriented_pair_inversion(perm, xi, i, xj, j)).len()
}

fn main() {
  let l1_inputs: [(usize, Vec<i32>); 5] = [
    (6, vec![3, 1, 6, 5, -2, 4]), // result: (2, [(1, -2), (3, -2)])
    (8, vec![0, 3, 1, 6, 5, -2, 4, 7]),
    (9, vec![3, 1, 6, 5, -2, 4, -7, 8, 9]),
    (8, vec![0, -5, -6, -1, -3, -2, 4, 7]),
    (193, vec![125, 133, 134, 135, 136, -52, -51, -50, -49, -48, -47, -46, -45, 66, 67, 68, 69,
               70, 71, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -132, -131, -130, -193,
               -192, -191, -190, -189, -188, -187, -186, -185, -184, -183, -182, -181, -180, -179,
               -178, -177, -176, -175, -174, -173, -172, -171, -170, -169, -77, -76, -75, -74, -73,
               -72, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, -164, -163, -65, -64, -63, -62,
               -61, -60, -59, -58, -57, -56, -55, -54, -53, 39, 40, 41, 42, 43, 44, 159, 160, 161,
               162, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1,
               -168, -167, -166, -165, 126, 127, 128, 129, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
               96, -124, -123, -122, -121, -120, -119, -118, -117, -116, -115, -114, -113, -112,
               -111, -110, -109, -108, -107, -106, -105, -104, -103, -102, -101, -100, -99, -98,
               -97, 153, 154, 155, 156, 157, 158, -148, -147, -146, -145, -144, -143, -142, -141,
               -140, -139, -138, -137, -85, -84, -83, -82, -81, -80, -79, -78, -152, -151, -150, -149]),
    ];

  for (n, perm) in &l1_inputs {
    assert_eq!(*n, perm.len());

    let pairs = oriented_pairs(perm);

    print!("{}", pairs.len());

    for (x, y) in pairs {
      print!(" {} {}", x, y);
    }

    println!();
  }

  println!("------------");

  type PairWithIndex = ((i32, usize), (i32, usize));

  let l2_inputs: [(usize, Vec<i32>, PairWithIndex); 5] = [
    (6, vec![3, 1, 6, 5, -2, 4], ((1, 1), (-2, 4))), // result: 3 1 2 -5 -6 4

    (6, vec![3, 1, 6, 5, -2, 4], ((3, 0), (-2, 4))), // result: -5 -6 -1 -3 -2 4

    (8, vec![0, 3, 1, 6, 5, -2, 4, 7], ((1, 2), (-2, 5))),

    (8, vec![0, 3, 1, 6, 5, -2, 4, 7], ((3, 1), (-2, 5))),

    (193, vec![125, 133, 134, 135, 136, -52, -51, -50, -49, -48, -47, -46, -45, 66, 67, 68, 69,
               70, 71, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -132, -131, -130, -193,
               -192, -191, -190, -189, -188, -187, -186, -185, -184, -183, -182, -181, -180, -179,
               -178, -177, -176, -175, -174, -173, -172, -171, -170, -169, -77, -76, -75, -74, -73,
               -72, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, -164, -163, -65, -64, -63, -62,
               -61, -60, -59, -58, -57, -56, -55, -54, -53, 39, 40, 41, 42, 43, 44, 159, 160, 161,
               162, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1,
               -168, -167, -166, -165, 126, 127, 128, 129, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
               96, -124, -123, -122, -121, -120, -119, -118, -117, -116, -115, -114, -113, -112,
               -111, -110, -109, -108, -107, -106, -105, -104, -103, -102, -101, -100, -99, -98,
               -97, 153, 154, 155, 156, 157, 158, -148, -147, -146, -145, -144, -143, -142, -141,
               -140, -139, -138, -137, -85, -84, -83, -82, -81, -80, -79, -78, -152, -151, -150, -149],
     ((-45, 12), (44, 94))),
  ];

  for (n, perm, ((xi, i), (xj, j))) in &l2_inputs {
    assert_eq!(*n, perm.len());
    let inverted = oriented_pair_inversion(perm, *xi, *i, *xj, *j);
    for e in inverted {
      print!("{} ", e);
    }
    println!();
  }

  for (n, perm, ((xi, i), (xj, j))) in &l2_inputs {
    assert_eq!(*n, perm.len());
    println!("Score = {}", score(perm, *xi, *i, *xj, *j));
  }

  let l4_inputs: [(usize, Vec<i32>); 2] = [
    (8, vec![0, 3, 1, 6, 5, -2, 4, 7]),
    (193, vec![125, 133, 134, 135, 136, -52, -51, -50, -49, -48, -47, -46, -45, 66, 67, 68, 69,
               70, 71, -38, -37, -36, -35, -34, -33, -32, -31, -30, -29, -132, -131, -130, -193,
               -192, -191, -190, -189, -188, -187, -186, -185, -184, -183, -182, -181, -180, -179,
               -178, -177, -176, -175, -174, -173, -172, -171, -170, -169, -77, -76, -75, -74, -73,
               -72, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, -164, -163, -65, -64, -63, -62,
               -61, -60, -59, -58, -57, -56, -55, -54, -53, 39, 40, 41, 42, 43, 44, 159, 160, 161,
               162, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1,
               -168, -167, -166, -165, 126, 127, 128, 129, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
               96, -124, -123, -122, -121, -120, -119, -118, -117, -116, -115, -114, -113, -112,
               -111, -110, -109, -108, -107, -106, -105, -104, -103, -102, -101, -100, -99, -98,
               -97, 153, 154, 155, 156, 157, 158, -148, -147, -146, -145, -144, -143, -142, -141,
               -140, -139, -138, -137, -85, -84, -83, -82, -81, -80, -79, -78, -152, -151, -150, -149]),
  ];

  for (n, perm) in &l4_inputs {
    assert_eq!(*n, perm.len());

    let mut inversions = 0;
    let mut perm: Vec<i32> = perm.to_vec();

    loop {
      let opairs: Vec<((i32, usize), (i32, usize))> = oriented_pairs_with_indexes(&perm);

      if opairs.is_empty() {
        break;
      }

      let pair_with_max_score: ((i32, usize), (i32, usize)) =
        *opairs.iter().max_by_key(|((xi, i), (xj, j))| score(&perm, *xi, *i, *xj, *j)).unwrap();

      let ((xi, i), (xj, j)) = pair_with_max_score;

      perm = oriented_pair_inversion(&perm, xi, i, xj, j);

      inversions += 1;
    }

    println!("Inversions necessary = {}", inversions);
  }
}
