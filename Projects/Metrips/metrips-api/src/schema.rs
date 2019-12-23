table! {
    trips (id) {
        id -> Int8,
        place -> Varchar,
        description -> Varchar,
        longitude -> Float8,
        latitude -> Float8,
    }
}

table! {
    users (id) {
        id -> Int8,
        first_name -> Varchar,
        last_name -> Varchar,
        email -> Varchar,
    }
}

allow_tables_to_appear_in_same_query!(
    trips,
    users,
);
