use std::ptr;

struct Foo {
  pub propa: String,
  pub propb: bool,
}

impl PartialEq for Foo {
  fn eq(&self, other: &Foo) -> bool {
    if self.propa == other.propa && self.propb == other.propb {
      println!("Foo::eq() -> they are equal!");
      return true;
    } else {
      println!("Foo::eq() -> they are not equal!");
      return false;
    }
  }
}

impl Eq for Foo {}

struct FooPtr<'a> {
  pub foo: &'a Foo,
}

impl<'a> PartialEq for FooPtr<'a> {
  fn eq(&self, other: &FooPtr) -> bool {
    self.foo as *const Foo == other.foo as *const Foo
  }
}

impl<'a> Eq for FooPtr<'a> {}

fn main() {
  let f1 = Foo {
    propa: "foo f1".to_owned(),
    propb: false,
  };

  let f1_ptr = FooPtr {
    foo: &f1
  };

  let f1_ptr_2 = FooPtr {
    foo: &f1
  };

  let f2 = Foo {
    propa: "foo f1".to_owned(),
    propb: false,
  };

  let f2_ptr = FooPtr {
    foo: &f2
  };

  println!(" ====> {}", f1_ptr == f2_ptr);
  println!(" ====> {}", f1_ptr == f1_ptr_2);

  // if f1 == f2 {
  //   println!("yes they are equal")
  // } else {
  //   println!("no not really equal")
  // }

  let f1_ref: &Foo = &f1;
  let f1_ptr: *const Foo = &f1;
  let f2_ref: &Foo = &f2;
  // let f2_ptr: *const Foo = &f2;

  if f1_ref == f2_ref {
    println!("their refs are equal too!")
  } else {
    println!("their refs are not equal!")
  }

  if f1_ref as *const Foo == f1_ptr {
    println!("pointers are equal")
  } else {
    println!("pointers are not equal")
  }

  if f1_ref as *const Foo == f2_ref as *const Foo {
    println!("pointers f1 and f2 are equal")
  } else {
    println!("pointers f1 and f2 are not equal")
  }

  if ptr::eq(f1_ref, f2_ref) {
    println!("std::ptr::eq() -> refs f1 and f2 are equal")
  } else {
    println!("std::ptr::eq() -> refs f1 and f2 are not equal")
  }

  let _f1_ref_2: &Foo = unsafe { &*f1_ptr };
}
