struct Phone {
  label: Option<usize>,
  value: Option<usize>,
}

struct Address {
  label: Option<usize>,
  street: Option<Vec<Option<usize>>>,
  city: Option<usize>,
  state: Option<usize>,
  postcode: Option<usize>,
  country: Option<usize>,
}

struct Extra {
  label: usize,
  value: Option<usize>,
}

pub struct Record {
  first_name: Option<usize>,
  last_name: Option<usize>,
  affixes: Option<Vec<Option<usize>>>,
  akas: Option<Vec<Option<usize>>>,
  organizations: Option<Vec<Option<usize>>>,
  phones: Option<Vec<Phone>>,
  addresses: Option<Vec<Address>>,
  emails: Option<Vec<Option<usize>>>,
  extras: Option<Vec<Extra>>,
  uuid: usize,
  creation_time: usize,
  update_time: usize,
  end_nil: usize,
}
