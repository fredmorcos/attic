#![warn(clippy::all)]

use std::collections::HashMap as Map;
use std::collections::HashSet as Set;
use std::fs;
use std::str::FromStr;

fn calc_fuel(mass: i32) -> i32 {
    ((f64::from(mass) / 3.0).floor() - 2.0) as i32
}

fn calc_extra_fuel(total: i32) -> i32 {
    let mut extra = calc_fuel(total);
    let mut total_extra = 0;
    while extra > 0 {
        total_extra += extra;
        extra = calc_fuel(extra);
    }
    total_extra
}

fn sol_1() {
    let data = fs::read_to_string("data/1.txt").unwrap();
    let mut total = 0;
    let mut total_extra = 0;
    for line in data.lines() {
        let val = i32::from_str(line).expect("Cannot parse value");
        let fuel = calc_fuel(val);
        total += fuel;
        total_extra += calc_extra_fuel(fuel);
    }
    println!("sol_1_1: {}", total);
    println!("sol_1_2: {}", total + total_extra);
}

fn interpret_intcode(values: &mut Vec<usize>) -> &mut Vec<usize> {
    let mut i = 0;
    loop {
        match values[i] {
            99 => break,
            1 | 2 => {
                let a = values[values[i + 1]];
                let b = values[values[i + 2]];
                let c = values[i + 3];
                values[c] = if values[i] == 1 { a + b } else { a * b };
                i += 4;
            }
            _ => panic!("Unknown opcode"),
        }
    }
    values
}

fn sol_2() {
    let data = fs::read_to_string("data/2.txt").unwrap();
    let original_values: Vec<usize> = (&data as &str)
        .trim()
        .split(',')
        .map(|v| usize::from_str(v).unwrap())
        .collect();

    let mut values = original_values.clone();
    values[1] = 12;
    values[2] = 2;
    let values = interpret_intcode(&mut values);
    println!("sol_2_1: {}", values[0]);

    'mainloop: for i in 0..original_values.len() {
        for j in 0..original_values.len() {
            let mut values = original_values.clone();

            values[1] = i;
            values[2] = j;

            let values = interpret_intcode(&mut values);
            if values[0] == 19_690_720 {
                println!("sol_2_2: {}", 100 * i + j);
                break 'mainloop;
            }
        }
    }
}

type Point = (isize, isize);

fn intersections(a_moves: &[&str], b_moves: &[&str]) -> Vec<Point> {
    fn trail(moves: &[&str]) -> Set<Point> {
        let mut current = (0, 0);
        let mut trail = Set::new();
        for m in moves {
            let m = m.split_at(1);
            let n = isize::from_str(m.1).unwrap();
            for _i in 1..=n {
                current = match m.0 {
                    "L" => (current.0 - 1, current.1),
                    "R" => (current.0 + 1, current.1),
                    "U" => (current.0, current.1 + 1),
                    "D" => (current.0, current.1 - 1),
                    _ => panic!("Unknown move direction"),
                };
                trail.insert(current);
            }
        }
        trail
    }

    let a_trail = trail(&a_moves);
    let b_trail = trail(&b_moves);

    a_trail.intersection(&b_trail).cloned().collect()
}

fn sol_3_1_slow() {
    fn dist((x, y): Point) -> isize {
        x.abs() + y.abs()
    }

    let data = fs::read_to_string("data/3.txt").unwrap();
    let moves: Vec<Vec<&str>> = (&data as &str)
        .trim()
        .lines()
        .map(|l| l.split(',').collect::<Vec<&str>>())
        .collect();
    let inters = intersections(&moves[0], &moves[1]);
    let mut closest_dist = dist(inters[0]);
    for intersection in inters {
        let new_dist = dist(intersection);
        if new_dist < closest_dist {
            closest_dist = new_dist;
        }
    }
    println!("sol_3_1_slow: {}", closest_dist);
}

fn intersection_dist(a_moves: &[&str], b_moves: &[&str]) -> isize {
    fn dist((x, y): Point) -> isize {
        x.abs() + y.abs()
    }

    let mut current = (0, 0);
    let mut a_trail = Set::new();
    for m in a_moves {
        let m = m.split_at(1);
        let n = isize::from_str(m.1).unwrap();
        for _i in 1..=n {
            current = match m.0 {
                "L" => (current.0 - 1, current.1),
                "R" => (current.0 + 1, current.1),
                "U" => (current.0, current.1 + 1),
                "D" => (current.0, current.1 - 1),
                _ => panic!("Unknown move direction"),
            };
            a_trail.insert(current);
        }
    }

    let mut current = (0, 0);
    let mut closest_dist = None;
    for m in b_moves {
        let m = m.split_at(1);
        let n = isize::from_str(m.1).unwrap();
        for _i in 1..=n {
            current = match m.0 {
                "L" => (current.0 - 1, current.1),
                "R" => (current.0 + 1, current.1),
                "U" => (current.0, current.1 + 1),
                "D" => (current.0, current.1 - 1),
                _ => panic!("Unknown move direction"),
            };
            if a_trail.contains(&current) {
                let new_dist = dist(current);
                if let Some(ref mut inner) = closest_dist {
                    if new_dist < *inner {
                        *inner = new_dist;
                    }
                } else {
                    closest_dist = Some(new_dist);
                }
            }
        }
    }

    closest_dist.unwrap()
}

fn sol_3_1() {
    let data = fs::read_to_string("data/3.txt").unwrap();
    let moves: Vec<Vec<&str>> = (&data as &str)
        .trim()
        .lines()
        .map(|l| l.split(',').collect::<Vec<&str>>())
        .collect();
    let closest_dist = intersection_dist(&moves[0], &moves[1]);
    println!("sol_3_1: {}", closest_dist);
}

fn count_steps(a_moves: &[&str], b_moves: &[&str]) -> usize {
    fn trail(moves: &[&str]) -> (Map<Point, usize>, Set<Point>) {
        let mut current = (0, 0);
        let mut trail = Set::new();
        let mut steps = 0;
        let mut trail_steps = Map::new();
        for m in moves {
            let m = m.split_at(1);
            let n = isize::from_str(m.1).unwrap();
            for _i in 1..=n {
                current = match m.0 {
                    "L" => (current.0 - 1, current.1),
                    "R" => (current.0 + 1, current.1),
                    "U" => (current.0, current.1 + 1),
                    "D" => (current.0, current.1 - 1),
                    _ => panic!("Unknown move direction"),
                };
                trail.insert(current);
                steps += 1;
                trail_steps.insert(current, steps);
            }
        }
        (trail_steps, trail)
    }

    let (a_trail_steps, a_trail) = trail(&a_moves);
    let (b_trail_steps, b_trail) = trail(&b_moves);

    let intersections: Vec<Point> = a_trail.intersection(&b_trail).cloned().collect();
    let mut shortest_intersection = None;

    for intersection in intersections {
        let a_steps = a_trail_steps.get(&intersection).unwrap();
        let b_steps = b_trail_steps.get(&intersection).unwrap();
        let steps = a_steps + b_steps;
        if let Some(ref mut inner) = shortest_intersection {
            if steps < *inner {
                *inner = steps;
            }
        } else {
            shortest_intersection = Some(steps);
        }
    }

    shortest_intersection.unwrap()
}

fn sol_3_2() {
    let data = fs::read_to_string("data/3.txt").unwrap();
    let moves: Vec<Vec<&str>> = (&data as &str)
        .trim()
        .lines()
        .map(|l| l.split(',').collect::<Vec<&str>>())
        .collect();
    let shortest_steps = count_steps(&moves[0], &moves[1]);
    println!("sol_3_2: {}", shortest_steps);
}

fn main() {
    sol_1();
    sol_2();
    sol_3_1();
    sol_3_1_slow();
    sol_3_2();
}

#[cfg(test)]
mod test {
    use crate::interpret_intcode;
    use crate::{calc_extra_fuel, calc_fuel};

    #[test]
    fn calc_fuel_test() {
        assert_eq!(calc_fuel(12), 2);
        assert_eq!(calc_fuel(14), 2);
        assert_eq!(calc_fuel(1969), 654);
        assert_eq!(calc_fuel(100_756), 33583);
    }

    #[test]
    fn calc_extra_fuel_test() {
        assert_eq!(calc_extra_fuel(14), 2);
        assert_eq!(calc_extra_fuel(1969), 654 + 216 + 70 + 21 + 5);
        assert_eq!(
            calc_extra_fuel(100_756),
            33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2
        );
    }

    #[test]
    fn interpret_intcode_test() {
        assert_eq!(
            interpret_intcode(&mut vec![1, 0, 0, 0, 99]),
            &mut vec![2, 0, 0, 0, 99]
        );
        assert_eq!(
            interpret_intcode(&mut vec![2, 3, 0, 3, 99]),
            &mut vec![2, 3, 0, 6, 99]
        );
        assert_eq!(
            interpret_intcode(&mut vec![2, 4, 4, 5, 99, 0]),
            &mut vec![2, 4, 4, 5, 99, 9801]
        );
        assert_eq!(
            interpret_intcode(&mut vec![1, 1, 1, 4, 99, 5, 6, 0, 99]),
            &mut vec![30, 1, 1, 4, 2, 5, 6, 0, 99]
        );
    }
}
