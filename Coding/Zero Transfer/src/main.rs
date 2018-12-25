fn transfer(arr: &mut [i8], from: usize, to: usize, amount: i8) {
  assert!(arr[from] < 0 || amount <= arr[from]);
  assert!(arr[to] > 0 || amount <= -arr[to]);

  arr[from] -= amount;
  arr[to] += amount;
}

fn zero(arr: &mut [i8]) {
  // this keeps the index to the element that holds the final "sum" of
  // positive numbers
  let mut pos_sum_idx: Option<usize> = None;

  // collect indexes of the positive numbers, works around not being
  // able to have 2 mutable references to arr
  let idxs: Vec<usize> = arr.iter()
                            .enumerate()
                            .filter(|(_, e)| **e > 0)
                            .map(|(i, _)| i)
                            .collect();

  for i in idxs {
    if let Some(idx) = pos_sum_idx {
      transfer(arr, idx, i, arr[idx]);
    }

    pos_sum_idx = Some(i);
  }

  // this keeps the index to the element that holds the final "sum" of
  // negative numbers
  let mut neg_sum_idx: Option<usize> = None;

  // collect indexes of the negative numbers, works around not being
  // able to have 2 mutable references to arr
  let idxs: Vec<usize> = arr.iter()
                            .enumerate()
                            .filter(|(_, e)| **e < 0)
                            .map(|(i, _)| i)
                            .collect();

  for i in idxs {
    if let Some(idx) = neg_sum_idx {
      transfer(arr, idx, i, arr[idx]);
    }

    neg_sum_idx = Some(i);
  }

  // All the elements were zero
  if pos_sum_idx.is_none() && neg_sum_idx.is_none() {
    return;
  }

  if let Some(pos_sum_idx) = pos_sum_idx {
    if let Some(neg_sum_idx) = neg_sum_idx {
      transfer(arr, neg_sum_idx, pos_sum_idx, arr[neg_sum_idx]);
    } else {
      unreachable!("No negative numbers in the array");
    }
  } else {
    unreachable!("No positive numbers in the array");
  }
}

fn main() {
  let mut arr = [1,2,3,-1,-2,-3];
  zero(&mut arr);
  assert_eq!(arr, [0,0,0,0,0,0]);

  let mut arr = [-1,1,-2,2,-3,3];
  zero(&mut arr);
  assert_eq!(arr, [0,0,0,0,0,0]);

  let mut arr = [0,0,0,0,0,0];
  zero(&mut arr);
  assert_eq!(arr, [0,0,0,0,0,0]);

  let mut arr = [-1,1,0,0,0,0];
  zero(&mut arr);
  assert_eq!(arr, [0,0,0,0,0,0]);

  let mut arr = [];
  zero(&mut arr);
  assert_eq!(arr, []);

  let mut arr = [-1,2,-1];
  zero(&mut arr);
  assert_eq!(arr, [0,0,0]);

  let mut arr = [0,-1,0,2,0,-1,0];
  zero(&mut arr);
  assert_eq!(arr, [0,0,0,0,0,0,0]);
}
