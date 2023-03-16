fn solution(n: i32) -> i32 {
    let s = n.to_string();
    let mut sum = 0;
    for c in s.chars() {
        sum += c.to_string().parse::<i32>().unwrap();
    }
    sum
}

fn binadd_nth_bit(x: &str, n: usize) -> u8 {
    let bytes = x.as_bytes();
    if n >= bytes.len() {
        return b'0';
    }
    bytes[bytes.len() - 1 - n]
}

fn bitadd(a: u8, b: u8, carry: u8) -> (u8, u8) {
    match (a, b, carry) {
        (b'0', b'0', b'0') => (b'0', b'0'),
        (b'0', b'0', b'1') => (b'1', b'0'),
        (b'0', b'1', b'0') => (b'1', b'0'),
        (b'0', b'1', b'1') => (b'0', b'1'),
        (b'1', b'0', b'0') => (b'1', b'0'),
        (b'1', b'0', b'1') => (b'0', b'1'),
        (b'1', b'1', b'0') => (b'0', b'1'),
        (b'1', b'1', b'1') => (b'1', b'1'),
        (_, _, _) => panic!("..."),
    }
}

fn binadd(a: String, b: String) -> String {
    let mut res = Vec::new();
    let mut carry = b'0';

    for i in 0..=a.len().max(b.len()) {
        let bit_a = binadd_nth_bit(&a, i);
        let bit_b = binadd_nth_bit(&b, i);

        dbg!(bit_a);
        dbg!(bit_b);
        dbg!(carry);

        let (v, new_carry) = bitadd(bit_a, bit_b, carry);
        res.push(v);
        carry = new_carry;
    }

    loop {
        let last = res.last().unwrap();
        if *last == b'0' {
            res.pop();
        } else {
            break;
        }
    }

    res.reverse();
    String::from_utf8(res).unwrap()
}

// fn boxes(length: Vec<i32>, width: Vec<i32>, height: Vec<i32>) -> bool {
//     todo!()
// }

fn median(mut sequence: Vec<i32>) -> f64 {
    sequence.sort();
    if sequence.len() % 2 == 0 {
        let med1 = sequence[sequence.len() / 2 - 1];
        let med2 = sequence[(sequence.len() / 2)];
        f64::from(med1 + med2) / 2.0
    } else {
        f64::from(sequence[sequence.len() / 2])
    }
}

fn cart(p: Vec<Vec<i32>>) -> f64 {
    let mut distance = f64::MAX;

    fn dist(point1_x: f64, point1_y: f64, point2_x: f64, point2_y: f64) -> f64 {
        let a = (point2_x - point1_x).powi(2);
        let b = (point2_y - point1_y).powi(2);
        (a + b).sqrt()
    }

    for i in 0..p.len() {
        // for (point1_idx, point1) in p.iter().enumerate() {
        let point1 = &p[i];
        let point1_x = f64::from(point1[0]);
        let point1_y = f64::from(point1[1]);

        for j in i + 1..p.len() {
            // for (point2_idx, point2) in p.iter().enumerate() {
            // if point1_idx == point2_idx {
            //     continue;
            // }

            let point2 = &p[j];
            let point2_x = f64::from(point2[0]);
            let point2_y = f64::from(point2[1]);

            let d = dist(point1_x, point1_y, point2_x, point2_y);
            if d < distance {
                distance = d;
            }
        }
    }

    distance
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = solution(92);
        assert_eq!(result, 11);
    }

    #[test]
    fn binadd_test() {
        assert_eq!(binadd_nth_bit("1101", 0), b'1');
        assert_eq!(binadd_nth_bit("1101", 1), b'0');
        assert_eq!(binadd_nth_bit("1101", 2), b'1');
        assert_eq!(binadd_nth_bit("1101", 3), b'1');
        assert_eq!(binadd_nth_bit("1101", 4), b'0');
        let result = binadd("1101".to_string(), "111".to_string());
        assert_eq!(result, "10100");
    }

    #[test]
    fn median_test() {
        assert_eq!(median(vec![-1, 3, -2, 2]), 0.5);
        assert_eq!(median(vec![1, 3, 2]), 2.0);
        assert_eq!(median(vec![1, 1]), 1.0);
        assert_eq!(median(vec![-50, 50, -25, 25, -1, 1, -100, 100]), 0.0);
    }

    #[test]
    fn cartes_test() {
        assert_eq!(
            cart(vec![vec![0, 11], vec![-7, 1], vec![-5, -3]]),
            4.47213595499958
        );
    }

    // #[test]
    // fn boxes_test() {
    //     assert_eq!(boxes(vec![1, 3, 2], vec![1, 3, 2], vec![1, 3, 2]), true);
    //     assert_eq!(boxes(vec![1, 1], vec![1, 1], vec![1, 1]), false);
    //     assert_eq!(boxes(vec![3, 1, 2], vec![3, 1, 2], vec![3, 2, 1]), false);
    //     assert_eq!(boxes(vec![2], vec![3], vec![4]), true);
    //     assert_eq!(
    //         boxes(
    //             vec![5, 7, 4, 1, 2],
    //             vec![4, 10, 3, 1, 4],
    //             vec![6, 5, 5, 1, 2]
    //         ),
    //         true
    //     );
    // }
}
