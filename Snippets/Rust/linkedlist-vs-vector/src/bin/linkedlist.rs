// extern crate cpuprofiler;
extern crate ll_vs_vec;

use ll_vs_vec::extras;
// use cpuprofiler::PROFILER;

fn main() {
  let list = extras::create_linkedlist(100_000_000);
  // PROFILER.lock()
  //         .unwrap()
  //         .start("./linkedlist.profile")
  //         .expect("Cannot start profiler");
  extras::traverse_linkedlist(&list);
  // PROFILER.lock()
  //         .unwrap()
  //         .stop()
  //         .expect("Cannot stop profiler");
  extras::drop_linkedlist(list);
}
