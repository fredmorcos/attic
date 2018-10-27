use std::fmt::*;

struct FSM {
  // pub name: String,
  pub states: Vec<State>,
}

// impl Display for FSM {
//   fn fmt(&self, f: &mut Formatter) -> Result {
//     write!(f, "FSM {} ({} states)", self.name, self.states.len())
//   }
// }

struct State {
  pub fsm_name: String,
  pub substates: Vec<Substate>,
}

impl Display for State {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "{} State ({} substates)", self.fsm_name, self.substates.len())
  }
}

impl Clone for State {
  fn clone(&self) -> Self {
    println!("FSM State being cloned!");
    State {
      fsm_name: self.fsm_name.clone(),
      substates: self.substates.clone(),
    }
  }
}

#[derive(Clone)]
struct Substate {
  pub sw: String,
  pub src: usize,
  pub dst: usize,
}

impl Display for Substate {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "Substate {} {} -> {}", self.sw, self.src, self.dst)
  }
}

#[inline]
fn combine_1<T: Copy>(list: &[T]) -> Vec<Vec<T>> {
  list.iter().map(|e| vec![*e]).collect()
}

#[inline]
fn combine_2<T: Copy>(list1: &[T], list2: &[T]) -> Vec<Vec<T>> {
  let mut res = Vec::with_capacity(list1.len() * list2.len());

  for e1 in list1 {
    for e2 in list2 {
      res.push(vec![*e1, *e2]);
    }
  }

  res
}

#[inline]
fn combine_3<T: Copy>(list1: &[T], list2: &[T], list3: &[T]) -> Vec<Vec<T>> {
  let mut res = Vec::with_capacity(list1.len() * list2.len() * list3.len());

  for e1 in list1 {
    for e2 in list2 {
      for e3 in list3 {
        res.push(vec![*e1, *e2, *e3]);
      }
    }
  }

  res
}

#[inline]
fn combine_n<T: Copy>(lists: &[Vec<T>]) -> Vec<Vec<T>> {
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
    let mut res = Vec::with_capacity(head.len() * rec_res.len());
    for e in head {
      for rec_vec in &rec_res {
        let mut new: Vec<T> = Vec::with_capacity(1 + rec_vec.len());
        new.push(*e);
        new.extend_from_slice(rec_vec);
        res.push(new);
      }
    }
    res
  }
}

fn main() {
  let fsms = vec![
    FSM {
      // name: "FSM1".to_string(),
      states: vec![
        State {
          fsm_name: "FSM1".to_string(),
          substates: vec![ Substate { sw: "Switch1".to_string(), src: 1, dst: 2 },
                           Substate { sw: "Switch1".to_string(), src: 3, dst: 4 },
                           Substate { sw: "Switch2".to_string(), src: 5, dst: 0 } ],
        },
        State {
          fsm_name: "FSM1".to_string(),
          substates: vec![ Substate { sw: "Switch1".to_string(), src: 1, dst: 6 },
                           Substate { sw: "Switch1".to_string(), src: 3, dst: 7 },
                           Substate { sw: "Switch2".to_string(), src: 5, dst: 8 } ],
        },
      ],
    },
    FSM {
      // name: "FSM2".to_string(),
      states: vec![
        State {
          fsm_name: "FSM2".to_string(),
          substates: vec![ Substate { sw: "Switch3".to_string(), src: 1, dst: 2 },
                           Substate { sw: "Switch3".to_string(), src: 3, dst: 4 },
                           Substate { sw: "Switch4".to_string(), src: 5, dst: 0 } ],
        },
        State {
          fsm_name: "FSM2".to_string(),
          substates: vec![ Substate { sw: "Switch3".to_string(), src: 1, dst: 6 },
                           Substate { sw: "Switch3".to_string(), src: 3, dst: 7 },
                           Substate { sw: "Switch4".to_string(), src: 5, dst: 8 } ],
        },
      ],
    },
  ];

  let vecs = vec![vec![0,1,2],
                  vec![0,1],
                  vec![0,1,2,3]];
  let res = combine_n(&vecs);
  for l in res {
    println!("{:?}", l);
  }

  let mut states: Vec<Vec<&State>> = vec![];

  for fsm in &fsms {
    let mut v = vec![];

    for state in &fsm.states {
      v.push(state);
    }

    states.push(v);
  }

  let res = combine_n(&states);

  for l in &res {
    for s in l {
      print!("{}, ", s);
    }
    println!("");
  }

  let vecs = vec![vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9],
                  vec![0,1,2,3,4,5,6,7,8,9]];
  let _res = combine_n(&vecs);
  // for l in res {
  //   println!("{:?}", l);
  // }
}
