use data::*;

fn combine_1<T: Clone>(list: &[T]) -> Vec<Vec<T>> {
  list.iter().map(|e| vec![e.clone()]).collect()
}

fn combine_2<T: Clone>(list1: &[T], list2: &[T]) -> Vec<Vec<T>> {
  let mut res = Vec::cap(list1.len() * list2.len());

  for e1 in list1 {
    for e2 in list2 {
      res.push(vec![e1.clone(), e2.clone()]);
    }
  }

  res
}

fn combine_3<T: Clone>(list1: &[T], list2: &[T], list3: &[T]) -> Vec<Vec<T>> {
  let mut res = Vec::cap(list1.len() * list2.len() * list3.len());

  for e1 in list1 {
    for e2 in list2 {
      for e3 in list3 {
        res.push(vec![e1.clone(), e2.clone(), e3.clone()]);
      }
    }
  }

  res
}

pub fn combine_n<T: Clone>(lists: &[Vec<T>]) -> Vec<Vec<T>> {
  if lists.is_empty() {
    vec![]
  } else if lists.len() == 1 {
    combine_1(&lists[0])
  } else if lists.len() == 2 {
    combine_2(&lists[0], &lists[1])
  } else if lists.len() == 3 {
    combine_3(&lists[0], &lists[1], &lists[2])
  } else {
    let (head, tail) = lists.split_first().unwrap();
    let rec_res = combine_n(tail);
    let mut res = Vec::cap(head.len() * rec_res.len());
    for e in head {
      for rec_vec in &rec_res {
        let mut new: Vec<T> = Vec::cap(1 + rec_vec.len());
        new.push(e.clone());
        new.extend_from_slice(rec_vec);
        res.push(new);
      }
    }
    res
  }
}

pub fn combine<T: Clone>(lists: &[Vec<T>]) -> Vec<Vec<T>> {
  fn inc(idxs: &mut Vec<usize>, max_idxs: &Vec<usize>) -> bool {
    if idxs == max_idxs {
      return false;
    }

    let mut i: usize = idxs.len() - 1;

    while i > 0 && idxs[i] == max_idxs[i] {
      i -= 1;
    }

    idxs[i] += 1;

    for i in i + 1 .. idxs.len() {
      idxs[i] = 0;
    }

    return true;
  }

  if lists.is_empty() {
    return vec![];
  }

  let mut total_len = 1;

  for l in lists {
    total_len *= l.len();
  }

  let mut res = vec![Vec::cap(lists.len()); total_len];

  let mut idxs: Vec<usize> = vec![0; lists.len()];
  let idxs_max: Vec<usize> = lists.iter().map(|l| l.len() - 1).collect();

  println!("Finished allocation");

  for mut v in &mut res {
    for i in 0 .. idxs.len() {
      v.push(lists[i][idxs[i]].clone());
    }

    if !inc(&mut idxs, &idxs_max) {
      break;
    }
  }

  res
}

pub fn merge_n<T: Copy>(head: &[T], tail: Vec<Vec<T>>) -> Vec<Vec<T>> {
  let mut res = vec![Vec::from(head); tail.len()];

  assert_eq!(res.len(), tail.len());

  for (mut h, t) in res.iter_mut().zip(tail) {
    h.extend(t)
  }

  res
}

pub fn merge<T: Copy>(head: T, tail: Vec<Vec<T>>) -> Vec<Vec<T>> {
  merge_n(&[head], tail)
}

#[cfg(test)]
mod test {
  use itertools::Itertools;

  #[test]
  fn itertools() {
    let v1 = vec![01,02,03,04,05,06,07,08,2];
    let v2 = vec![08,09,10,11,12,10,11,12,2];
    let v3 = vec![13,14,15,16,17,18,19,32,2];
    let v4 = vec![20,21,22,23,24,25,32,33,2];
    let v5 = vec![26,27,28,29,30,31,32,33,2];
    let v6 = vec![26,27,28,29,30,31,32,33,2];
    let v7 = vec![26,27,28,29,30,31,32,33,2];
    let v8 = vec![26,27,28,29,30,31,32,33,2];
    let v9 = vec![08,09,10,11,12,10,11,12,2];
    let v: Vec<Vec<u8>> = vec![v1,v2,v3,v4,v5,v6,v7,v8,v9];
    for r in v.iter().multi_cartesian_product() {
    }
  }

  #[test]
  fn combine_1() {
    let v = vec![1,2,3];
    let r = ::comb::combine_1(&v);
    assert_eq!(r, vec![vec![1],
                       vec![2],
                       vec![3]]);
  }

  #[test]
  fn combine_2() {
    let v1 = vec![1,2,3];
    let v2 = vec![4,5,6];
    let r = ::comb::combine_2(&v1, &v2);
    assert_eq!(r, vec![vec![1,4], vec![1,5], vec![1,6],
                       vec![2,4], vec![2,5], vec![2,6],
                       vec![3,4], vec![3,5], vec![3,6]]);
  }

  #[test]
  fn combine_3() {
    let v1 = vec![1,2];
    let v2 = vec![3,4];
    let v3 = vec![5,6];
    let r = ::comb::combine_3(&v1, &v2, &v3);
    assert_eq!(r, vec![vec![1,3,5], vec![1,3,6], vec![1,4,5], vec![1,4,6],
                       vec![2,3,5], vec![2,3,6], vec![2,4,5], vec![2,4,6]]);
  }

  #[test]
  fn combine_n() {
    let v1 = vec![1,2];
    let v2 = vec![3,4];
    let v3 = vec![5,6];
    let v4 = vec![7,8];
    let r = ::comb::combine_n(&[v1, v2, v3, v4]);
    assert_eq!(r, vec![vec![1,3,5,7], vec![1,3,5,8],
                       vec![1,3,6,7], vec![1,3,6,8],
                       vec![1,4,5,7], vec![1,4,5,8],
                       vec![1,4,6,7], vec![1,4,6,8],
                       vec![2,3,5,7], vec![2,3,5,8],
                       vec![2,3,6,7], vec![2,3,6,8],
                       vec![2,4,5,7], vec![2,4,5,8],
                       vec![2,4,6,7], vec![2,4,6,8]]);
  }

  #[test]
  fn combine_iter_1() {
    let v1 = vec![1,2];
    let v2 = vec![3,4];
    let v3 = vec![5,6];
    let v4 = vec![7,8];
    let r = ::comb::combine(&[v1, v2, v3, v4]);
    assert_eq!(r, vec![vec![1,3,5,7], vec![1,3,5,8],
                       vec![1,3,6,7], vec![1,3,6,8],
                       vec![1,4,5,7], vec![1,4,5,8],
                       vec![1,4,6,7], vec![1,4,6,8],
                       vec![2,3,5,7], vec![2,3,5,8],
                       vec![2,3,6,7], vec![2,3,6,8],
                       vec![2,4,5,7], vec![2,4,5,8],
                       vec![2,4,6,7], vec![2,4,6,8]]);
  }

  #[test]
  fn combine_iter_2() {
    let v1 = vec![1,2,3];
    let v2 = vec![4,5];
    let v3 = vec![6,7];
    let r = ::comb::combine(&[v1, v2, v3]);
    assert_eq!(r, vec![vec![1,4,6], vec![1,4,7],
                       vec![1,5,6], vec![1,5,7],
                       vec![2,4,6], vec![2,4,7],
                       vec![2,5,6], vec![2,5,7],
                       vec![3,4,6], vec![3,4,7],
                       vec![3,5,6], vec![3,5,7]]);
  }

  #[test]
  fn combine_n_huge() {
    let v1 = vec![01,02,03,04,05,06,07,08,2];
    let v2 = vec![08,09,10,11,12,10,11,12,2];
    let v3 = vec![13,14,15,16,17,18,19,32,2];
    let v4 = vec![20,21,22,23,24,25,32,33,2];
    let v5 = vec![26,27,28,29,30,31,32,33,2];
    let v6 = vec![26,27,28,29,30,31,32,33,2];
    let v7 = vec![26,27,28,29,30,31,32,33,2];
    let v8 = vec![26,27,28,29,30,31,32,33,2];
    let v9 = vec![08,09,10,11,12,10,11,12,2];
    let v: Vec<Vec<u8>> = vec![v1, v2, v3, v4, v5, v6, v7, v8, v9];
    let _ = ::comb::combine_n(&v);
    // println!("{:?}", r);
  }

  #[test]
  fn combine_iter_huge() {
    let v1 = vec![01,02,03,04,05,06,07,08,2];
    let v2 = vec![08,09,10,11,12,10,11,12,2];
    let v3 = vec![13,14,15,16,17,18,19,32,2];
    let v4 = vec![20,21,22,23,24,25,32,33,2];
    let v5 = vec![26,27,28,29,30,31,32,33,2];
    let v6 = vec![26,27,28,29,30,31,32,33,2];
    let v7 = vec![26,27,28,29,30,31,32,33,2];
    let v8 = vec![26,27,28,29,30,31,32,33,2];
    let v9 = vec![08,09,10,11,12,10,11,12,2];
    let v: Vec<Vec<u8>> = vec![v1, v2, v3, v4, v5, v6, v7, v8, v9];
    let _ = ::comb::combine(&v);
    // println!("{:?}", r);
  }

  #[test]
  fn combine_n_2() {
    let v1 = vec![1,2];
    let v2 = vec![3,4,5];
    let v3 = vec![6];
    let r = ::comb::combine_n(&[v1, v2, v3]);
    assert_eq!(r, vec![vec![1,3,6], vec![1,4,6], vec![1,5,6],
                       vec![2,3,6], vec![2,4,6], vec![2,5,6]]);
  }

  #[test]
  fn combine_n_empty() {
    let v: Vec<Vec<u8>> = ::comb::combine_n(&[]);
    assert!(v.is_empty());
  }

  #[test]
  fn merge_n() {
    let v = vec![vec![2,3,4],
                 vec![5,6,7,8,9],
                 vec![8,9,0]];
    let r = ::comb::merge_n(&[0, 1, 2], v);
    assert_eq!(r, vec![vec![0,1,2,2,3,4],
                       vec![0,1,2,5,6,7,8,9],
                       vec![0,1,2,8,9,0]]);
  }

  #[test]
  fn merge() {
    let v = vec![vec![2,3,4],
                 vec![5,6,7,8,9],
                 vec![8,9,0]];
    let r = ::comb::merge(1, v);
    assert_eq!(r, vec![vec![1,2,3,4],
                       vec![1,5,6,7,8,9],
                       vec![1,8,9,0]]);
  }

  #[test]
  fn merge_n_empty_head() {
    let v = vec![vec![2,3,4],
                 vec![5,6,7,8,9],
                 vec![8,9,0]];
    let v2 = v.clone();
    let r = ::comb::merge_n(&[], v);
    assert_eq!(r, v2);
  }

  #[test]
  fn merge_n_empty_tail() {
    let v = vec![];
    let r = ::comb::merge_n(&[0, 1, 2], v);
    assert!(r.is_empty());
  }

  #[test]
  fn merge_empty_tail() {
    let v = vec![];
    let r = ::comb::merge(1, v);
    assert!(r.is_empty());
  }

  #[test]
  fn merge_n_single_tail() {
    let v = vec![vec![2,3,4]];
    let r = ::comb::merge_n(&[0, 1, 2], v);
    assert_eq!(r, vec![vec![0,1,2,2,3,4]]);
  }

  #[test]
  fn merge_single_tail() {
    let v = vec![vec![2,3,4]];
    let r = ::comb::merge(1, v);
    assert_eq!(r, vec![vec![1,2,3,4]]);
  }
}
