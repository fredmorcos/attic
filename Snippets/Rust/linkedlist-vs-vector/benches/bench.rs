#[macro_use]
extern crate criterion;
extern crate ll_vs_vec;

use ll_vs_vec::extras;

use criterion::{Criterion, Fun};

fn ll_vs_vec(n: u32, c: &mut Criterion) {
  let linkedlist = Fun::new("linkedlist", |b, n| {
    let list = extras::create_linkedlist(*n);
    let res = b.iter(|| extras::traverse_linkedlist(&list));
    extras::drop_linkedlist(list);
    res
  });

  let vector_iter = Fun::new("vector_iter", |b, n| {
    let vec = extras::create_vector(*n);
    b.iter(|| extras::traverse_vector_iter(&vec))
  });

  let vector_index = Fun::new("vector_index", |b, n| {
    let vec = extras::create_vector(*n);
    b.iter(|| extras::traverse_vector_index(&vec))
  });

  let vector_unchecked = Fun::new("vector_unchecked", |b, n| {
    let vec = extras::create_vector(*n);
    b.iter(|| extras::traverse_vector_unchecked(&vec))
  });

  c.bench_functions(&format!("ll_vs_vec/{}", n),
                    vec![linkedlist, vector_iter,
                         vector_index, vector_unchecked], n);
}

macro_rules! ll_vs_vec_n {
  ($f:ident, $n:expr) => {
    fn $f(c: &mut Criterion) {
      ll_vs_vec($n, c);
    }
  }
}

ll_vs_vec_n!(f_100        , 100);
ll_vs_vec_n!(f_1_000      , 1_000);
ll_vs_vec_n!(f_10_000     , 10_000);
ll_vs_vec_n!(f_100_000    , 100_000);
ll_vs_vec_n!(f_1_000_000  , 1_000_000);
ll_vs_vec_n!(f_10_000_000 , 10_000_000);
ll_vs_vec_n!(f_100_000_000, 100_000_000);

criterion_group! {
  name = benches;
  config = Criterion::default().sample_size(5);
  targets = f_100, f_1_000, f_10_000, f_100_000,
            f_1_000_000, f_10_000_000, f_10_000_000, f_100_000_000
}

criterion_main!(benches);
