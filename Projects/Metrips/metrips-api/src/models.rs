use crate::schema::{trips, users};
use diesel::Queryable;
use serde::Serialize;

#[derive(Queryable, Serialize)]
pub struct User {
    // TODO IDs will be UUIDs (String)
    pub id: i64,
    #[serde(rename = "firstName")]
    pub first_name: String,
    #[serde(rename = "lastName")]
    pub last_name: String,
    pub email: String,
}

#[derive(Insertable)]
#[table_name = "users"]
pub struct NewUser<'a> {
    pub first_name: &'a str,
    pub last_name: &'a str,
    pub email: &'a str,
}

#[derive(Queryable, Serialize)]
pub struct Trip {
    // TODO IDs will be UUIDs (String)
    pub id: i64,
    pub place: String,
    pub description: String,
    pub longitude: f64,
    pub latitude: f64,
}

#[derive(Insertable)]
#[table_name = "trips"]
pub struct NewTrip<'a> {
    pub place: &'a str,
    pub description: &'a str,
    pub longitude: f64,
    pub latitude: f64,
}
