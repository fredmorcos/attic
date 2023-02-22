pub fn run(input: &str) {
  let measurements = buff_to_vec(input);
  println!("  Part 1:");
  println!("    Sonar Sweep: {}", sonar_sweep(&measurements));
  println!("  Part 2:");
  println!("    Sonar Sweep Filtered: {}", sonar_sweep_filtered(&measurements));
}

fn sonar_sweep(measurements: &[u32]) -> usize {
  measurements.windows(2).fold(0, |acc, win| {
    if win[0] < win[1] {
      acc + 1
    } else {
      acc
    }
  })
}

fn sonar_sweep_filtered(measurements: &[u32]) -> usize {
  measurements.windows(4).filter(|win| win[3] > win[0]).count()
}

fn buff_to_vec(input: &str) -> Vec<u32> {
  let mut res = vec![];

  for line in input.lines() {
    res.push(line.parse::<u32>().unwrap());
  }

  res
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn sonar_sweep_returns_2_increases() {
    let measurements = vec![199, 200, 208];
    assert_eq!(sonar_sweep(&measurements), 2);
  }

  #[test]
  fn sonar_sweep_returns_3_increases() {
    let measurements = vec![199, 200, 208, 210];
    assert_eq!(sonar_sweep(&measurements), 3);
  }

  #[test]
  fn sonar_sweep_returns_7_increases() {
    let measurements = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
    assert_eq!(sonar_sweep(&measurements), 7);
  }

  #[test]
  fn sonar_sweep_returns_0_increases_on_emtpy_array() {
    let measurements = vec![];
    assert_eq!(sonar_sweep(&measurements), 0);
  }

  #[test]
  fn sonar_sweep_returns_amount_of_increases() {
    let s = include_str!("../puzzles/day01_input_Fred.txt"); // "198\n201\n208\n..."
    let measurements = buff_to_vec(s);
    assert_eq!(sonar_sweep(&measurements), 1266);
  }

  #[test]
  fn filter_sonar_sweep_returns_5_increases() {
    let measurements = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
    assert_eq!(sonar_sweep_filtered(&measurements), 5);
  }

  #[test]
  fn sonar_sweep_filter_returns_amount_of_increases() {
    let s = include_str!("../puzzles/day01_input_Fred.txt"); // "198\n201\n208\n..."
    let measurements = buff_to_vec(s);
    assert_eq!(sonar_sweep_filtered(&measurements), 1217);
  }

  #[test]
  fn sonar_sweep_filtered_returns_0_increases() {
    let measurements = vec![199, 200, 208];
    assert_eq!(sonar_sweep_filtered(&measurements), 0);
  }
}
