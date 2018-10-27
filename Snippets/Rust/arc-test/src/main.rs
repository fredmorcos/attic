use std::sync::Arc;
use std::sync::Weak;

struct Foo {
  foo: Arc<usize>,
  bar: Arc<usize>,
}

struct Bar {
  foo: Weak<usize>,
  bar: Weak<usize>,
}

struct Parent {
  child: Child,
}

struct Child {
  parent: Weak<Parent>,
}

fn main() {
  let v = Arc::new(5);

  let b = {
    let mut f = Foo { foo: Arc::clone(&v), bar: Arc::clone(&v), };
    f.bar = f.foo.clone();
    println!("Foo {} {}", f.foo, f.bar);
    Bar { foo: Arc::downgrade(&v), bar: Arc::downgrade(&v) }
  };

  println!("Bar {} {}", b.foo.upgrade().unwrap(), b.bar.upgrade().unwrap());

  let mut p = Arc::new(Parent { child: Child { parent: Weak::new() } });
  let parent = Arc::downgrade(&p);
  p.child.parent = parent;
}
