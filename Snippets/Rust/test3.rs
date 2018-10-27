#[derive(Clone, Debug)]
struct Person {
  name: String,
  age: u8,
}

#[derive(Clone, Debug)]
struct PersonRef<'a> {
  name: &'a str,
  age: u8
}

fn copy_person<'a>(person: &'a Person) -> PersonRef<'a> {
  PersonRef {
    name: &person.name,
    age: person.age,
  }
}

fn new_person(name: String, age: u8) -> Person {
  Person { name, age }
}

fn get_age_byvalue(person: Person) -> u8 {
  person.age
}

fn get_age_byref(person: &Person) -> &u8 {
  &person.age
}

// fn set_age_byval(mut person: Person, age: u8) -> Person {
fn set_age_byval(person: Person, age: u8) -> Person {
  let mut person = person;
  person.age = age;
  person
}

fn set_age_byref(person: &mut Person, age: u8) {
  person.age = age;
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test() {
    // let kevin = Person {
    //   name: "Kevin".to_owned(),
    //   age: 250,
    // };

    let kevin = new_person("Kevin".to_owned(), 250);

    {
      let age: &u8 = get_age_byref(&kevin);
      println!("Name = {}, age = {}", kevin.name, age);
      // age the &u8 dies here
    }

    // we can now move kevin
    // let age = get_age_byvalue(kevin);

    let _ = get_age_byref(&kevin);
    println!("Name = {}, age = {}", kevin.name, kevin.age);
    let _ = get_age_byvalue(kevin);

    let kevin = new_person("Kevin".to_owned(), 250);
    let mut kevin = set_age_byval(set_age_byval(kevin, 21), 32);
    println!("Name = {}, age = {}", kevin.name, kevin.age);
    set_age_byref(&mut kevin, 64);
    println!("Name = {}, age = {}", kevin.name, kevin.age);

    let name = &kevin.name;
    println!("Name = {}, age = {}, nameref = {}", kevin.name, kevin.age, name);

    println!("Kevin = {:?}", kevin);

    let mut kevin2 = kevin.clone();
    kevin2.age = 128;
    println!("Name = {}, age = {}", kevin2.name, kevin2.age);

    // let n: &mut str = &mut kevin2.name;
    let n: &str = &kevin2.name;

    let kevin3: PersonRef = copy_person(&kevin2);
    println!("Name = {}, age = {}", kevin3.name, kevin3.age);

    // kevin2.age = 15;
    // kevin2.name = String::from("Kevin2");

    let kevin4: PersonRef = copy_person(&kevin2);
    println!("Name = {}, age = {}", kevin4.name, kevin4.age);
  }
}
