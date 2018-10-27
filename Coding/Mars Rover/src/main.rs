#![feature(tool_lints)]
#![warn(clippy::all)]

struct Rover {
  wheel_base: f64,
  max_steering_angle: f64,
}

impl Rover {
  fn new(wheel_base: f64, max_steering_angle: f64) -> Self {
    Self {
      wheel_base: wheel_base,
      max_steering_angle: max_steering_angle,
    }
  }

  fn turn_radius(&self, steering_angle: f64) -> f64 {
    self.wheel_base / steering_angle.to_radians().sin()
  }

  fn perimiter(&self, steering_angle: f64) -> f64 {
    2.0 * std::f64::consts::PI * self.turn_radius(steering_angle)
  }

  fn angle(&self, steering_angle: f64, distance: f64) -> f64 {
    let mut angle = (distance * 360.0) / self.perimiter(steering_angle);

    while angle < 0.0 {
      angle += 360.0;
    }

    while angle >= 360.0 {
      angle -= 360.0;
    }

    angle
  }

  fn position(&self, steering_angle: f64, distance: f64) -> (f64, f64) {
    if steering_angle == 0.0 {
      (0.0, distance)
    } else {
      let angle = self.angle(steering_angle, distance);
      let r = self.turn_radius(steering_angle);
      let x = r - (angle.to_radians().cos() * r);
      let y = angle.to_radians().sin() * r;
      (x, y)
    }
  }
}

fn main() {
  let l1_inputs: [(f64, f64); 5] = [
    (1.00, 30.00),
    (1.00, 13.76),
    (1.00, 2.34),
    (1.00, 90.00),
    (2.45, 90.00),
  ];

  for (wheel_base, steering_angle) in &l1_inputs {
    println!("Wheel Base, Steering Angle, Turn Radius = {:.2} {:.2} {:.2}",
             wheel_base, steering_angle,
             Rover::new(*wheel_base, 0.0).turn_radius(*steering_angle));
  }

  let l2_input: [(f64, f64, f64); 6] = [
    (1.00,  1.00,   30.00),
    (2.13,  4.30,   23.00),
    (1.75,  3.14,  -23.00),
    (2.70,  45.00, -34.00),
    (4.20, -5.30,   20.00),
    (9.53,  8.12,   0.00),
  ];

  for (wheel_base, distance, steering_angle) in &l2_input {
    let rover = Rover::new(*wheel_base, 0.0);
    let (x, y) = rover.position(*steering_angle, *distance);
    let angle = rover.angle(*steering_angle, *distance);
    println!("X, Y, Direction = {:.2} {:.2} {:.2}", x, y, angle);
  }

  type Segment = (f64, f64);
  type Segments = Vec<Segment>;
  type Input = (f64, usize, Segments);

  let l3_inputs: [Input; 8] = [
    (1.00, 1, vec![(5.00, 23.00)]),     // result: 3.52 2.37 111.94

    (1.00, 3, vec![(6.00, 23.00),       // 4.35 1.83 134.32
                   (10.00, -23.00),     //
                   (23.50, 23.00)]),    // result:

    (0.50, 2, vec![(10.00, 0.00),       // 0.00 10.00 0.00
                   (500.00, 3.00)]),    // result:

    (2.70, 3, vec![(5.00, 10.00),       // 0.80 4.91 18.42
                   (5.00, -10.00),      //
                   (20.00, 0.00)]),     // result:

    (4.20, 1, vec![(-100.00, -12.00)]), // result: -15.44 19.63 283.63

    (9.53, 10, vec![(-1.00, 1.00),      // -15.44 19.63 283.63
                    (-2.00, 2.00),      //
                    (3.00, -3.00),      //
                    (4.00, 4.00),       //
                    (5.00, -5.00),      //
                    (6.00, 6.00),       //
                    (7.00, 7.00),       //
                    (-8.00, 8.00),      //
                    (9.00, 9.00),       //
                    (10.00, -10.00)]),  // result:

    (1.00, 3, vec![(1.00, 15.00),       // 0.13 0.99 14.83
                   (1.00, 0.00),        //
                   (1.00, -15.00)]),    // result: 0.51 2.94 0.00

    (1.09, 4, vec![(9.86, 10.00),       // 6.28 6.28 90.00
                   (9.86, 10.00),       // 12.55 0.00 180.00
                   (9.86, 10.00),       // 6.28 -6.28 270.00
                   (9.86, 10.00)]),     // result: 0.00 0.00 0.00
    ];

  for (i, (wheel_base, n, segments)) in l3_inputs.into_iter().enumerate() {
    assert_eq!(segments.len(), *n);

    println!("Test {} -----------------------", i);

    let rover: Rover = Rover::new(*wheel_base, 0.0);
    let mut x: f64 = 0.0;
    let mut y: f64 = 0.0;
    let mut angle: f64 = 0.0;

    for (j, (distance, steering_angle)) in segments.into_iter().enumerate() {
      let (x_, y_) = rover.position(*steering_angle, *distance);
      let angle_ = rover.angle(*steering_angle, *distance);

      x += (angle.to_radians().cos() * x_) + (angle.to_radians().sin() * y_);
      y += (- angle.to_radians().sin() * x_) + (angle.to_radians().cos() * y_);

      angle += angle_;

      while angle < 0.0 {
        angle += 360.0;
      }

      while angle >= 360.0 {
        angle -= 360.0;
      }

      println!("Position after move {}: X, Y, Direction = {:.2} {:.2} {:.2}",
               j, x, y, angle);
    }

    println!("X, Y, Direction = {:.2} {:.2} {:.2}", x, y, angle);
    println!();
  }
}
