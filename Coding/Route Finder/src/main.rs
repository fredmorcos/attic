#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;

extern crate reqwest;
extern crate haversine;
extern crate structopt;

use structopt::StructOpt;
use std::fs::File;
use std::collections::hash_map::HashMap as Map;

#[derive(Debug, Deserialize)]
pub struct Coord {
  id: usize,
  latitude: f64,
  longitude: f64,
  connections: Vec<usize>,
}

#[derive(Deserialize)]
pub struct Link {
  next: Option<String>,
}

#[derive(Deserialize)]
pub struct Resp {
  coordinates: Vec<Coord>,
  links: Link,
}

fn get_coords(url: String, res: &mut Map<usize, Coord>) {
  // println!("Requesting {}...", url);

  let body: String = reqwest::get(&url).unwrap().text().unwrap();
  let resp: Resp = serde_json::from_str(&body).unwrap();

  for coord in resp.coordinates {
    let id = coord.id;

    if let Some(_) = res.insert(id, coord) {
      panic!("Coordinate with ID {} already in map", id);
    }
  }

  // println!("Res now has {} elements", res.len());

  if let Some(next_url) = resp.links.next {
    // println!("Found a next link: {}", next_url);
    get_coords(next_url, res);
  }
}

fn get_neighbors<'a>(coord: &Coord,
                     coords: &'a Map<usize, Coord>,
                     res: &mut Vec<&'a Coord>)
{
  for conn in &coord.connections {
    res.push(coords.get(conn).unwrap());
  }
}

fn dist(a: &Coord, b: &Coord) -> f64 {
  let aloc = haversine::Location {
    latitude: a.latitude,
    longitude: a.longitude,
  };

  let bloc = haversine::Location {
    latitude: b.latitude,
    longitude: b.longitude,
  };

  (haversine::distance(aloc, bloc, haversine::Units::Kilometers)
   * 1000.0).round()
}

fn flatten<'a>(coord: &'a Coord,
               coords: &'a Map<usize, Coord>,
               mut visited: Map<usize, bool>)
               -> Vec<Vec<&'a Coord>>
{
  let mut res = Vec::new();
  let mut neighbors = Vec::new();

  // print!("Getting neighbors...");

  get_neighbors(coord, coords, &mut neighbors);

  // println!(" {}", neighbors.len());

  if let Some(_) = visited.insert(coord.id, true) {
    panic!("Visiting an already visited coord");
  }

  if neighbors.is_empty() {
    vec![vec![coord]]
  } else {
    for neigh in neighbors {
      if let Some(_) = visited.get(&neigh.id) {
        continue;
      }

      let mut routes = flatten(neigh, coords, visited.clone());

      for route in &mut routes {
        route.insert(0, coord);
      }

      res.extend(routes.into_iter());
    }

    res
  }
}

fn total_distance(route: &[&Coord]) -> f64 {
  let mut sum = 0.0;

  for i in 1 .. route.len() {
    sum += dist(route[i], route[i - 1]);
  }

  sum
}

#[derive(StructOpt, Debug)]
pub struct Opt {
  #[structopt(short = "d",
              long = "descending",
              help = "Show routes in descending order")]
  descending: bool,
}

fn main() {
  let opt = Opt::from_args();

  let url = "https://meet-and-code-2018.herokuapp.com/coordinates";
  let mut coords: Map<usize, Coord> = Map::new();

  get_coords(url.to_string(), &mut coords);

  let coord_14: &Coord = coords.get(&14).unwrap();
  let mut neighbors = Vec::new();

  get_neighbors(coord_14, &coords, &mut neighbors);

  // println!("Coord 14 has {} neighbors", neighbors.len());

  // let mut sum: f64 = 0.0;

  // for neigh in neighbors {
  //   let dist = dist(coord_14, neigh);
  //   println!("Distance between Coord 14 and Coord {} is {} M",
  //            neigh.id, dist);
  //   sum += dist;
  // }

  // println!("Sum of all distances to neighbors is {} M", sum);

  let n_mondays = 53.0;
  let goal: f64 = 510.0 * 1000.0;
  let coord_1: &Coord = coords.get(&1).unwrap();

  // println!("Coord 1 neighbors = {:?}", coord_1.connections);

  let visited: Map<usize, bool> = Map::new();
  let routes: Vec<Vec<&Coord>> = flatten(coord_1,
                                         &coords,
                                         visited);

  let total_distances: Vec<f64> = routes
    .iter()
    .map(|route| total_distance(route))
    .collect();

  // println!("Found {} routes", total_distances.len());

  let mut valid_routes: Vec<(Vec<&Coord>, f64)> = routes
    .into_iter()
    .enumerate()
    .filter(|(i, _)| total_distances[*i] * n_mondays >= goal)
    .map(|(i, r)| (r, total_distances[i]))
    .collect();

  // println!("Found {} valid routes", valid_routes.len());
  // println!("The total goal: {} M", goal);

  if opt.descending {
    valid_routes.sort_by(
      |(_, d1), (_, d2)|
      ((d2 * n_mondays) - goal).partial_cmp(
        &((d1 * n_mondays) - goal)).unwrap());
  } else {
    valid_routes.sort_by(
      |(_, d1), (_, d2)|
      ((d1 * n_mondays) - goal).partial_cmp(
        &((d2 * n_mondays) - goal)).unwrap());
  }

  // for route in valid_routes {
  //   // println!("{:?}", route);
  //   print!("Route: {},", route.0[0].id);

  //   for coord in route.0.iter().skip(1) {
  //     print!(" {},", coord.id);
  //   }

  //   println!(" {}", route.1);
  // }

  let avatar_url =
    "https://meet-and-code-2018.herokuapp.com/routes/avatars?rank=";

  for (i, route) in valid_routes.iter().enumerate() {
    let mut f: File = File::create("avatar.png").unwrap();
    reqwest::get(&format!("{}{}", avatar_url, i + 1))
      .unwrap().copy_to(&mut f).unwrap();

    std::process::Command::new("/usr/bin/tycat")
      .arg("avatar.png")
      .spawn()
      .expect("Cannot execute command");

    println!("Route {}: {} coordinates - {}m",
             i + 1, route.0.len(), route.1);
  }
}
