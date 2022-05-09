#![warn(clippy::all)]

use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("data/input_3_1.txt")?;
    let cts = BufReader::new(file);
    calc_power(cts.lines())?;
    Ok(())
}

fn calc_power(lines: impl Iterator<Item = io::Result<String>>) -> Result<(), Box<dyn Error>> {
    let mut count_ones = vec![];
    let mut n_lines = 0;

    for line in lines {
        for (idx, &c) in line?.as_bytes().iter().enumerate() {
            if count_ones.len() <= idx {
                count_ones.push(0_usize);
            }

            if c == b'1' {
                count_ones[idx] += 1;
            }
        }

        n_lines += 1;
    }

    let mut gamma = 0;
    let mut epsilon = 0;

    for n_ones in count_ones {
        if dbg!(n_ones) >= n_lines / 2 {
            gamma |= 1;
        } else {
            epsilon |= 1;
        }

        gamma <<= 1;
        epsilon <<= 1;
    }

    gamma >>= 1;
    epsilon >>= 1;

    println!("Gamma: {}", gamma);
    println!("Epsilon: {}", epsilon);
    println!("Final: {}", gamma * epsilon);

    Ok(())
}
