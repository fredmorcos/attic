use std::{thread::sleep, time::Duration};

use crate::throttled_func_mod::get_throttled_func;

// Part 1:
//
// Given an array of integers and a value, write a function which can determine if there
// are any three distinct elements in the array whose sum equals the given value.
//
// If a match is found, print the three integers and return true. Otherwise, return false.
//
// Part 2:
//
// Modify your application such that your new function cannot be invoked more than 10
// times per second.
//
// Invocations which exceed this limit can be discarded.
//
// Part 3:
//
// Extend your function to determine if there are any N integers in the array whose sum
// equals the given value.
//
// If a match is found, print the N integers and return true. Otherwise, return false.

fn match_three(arr: &[i32], val: i32) -> bool {
    for i in 0..arr.len() {
        for j in i + 1..arr.len() {
            for k in j + 1..arr.len() {
                if arr[i] + arr[j] + arr[k] == val {
                    println!("match_three: {} {} {}", arr[i], arr[j], arr[k]);
                    return true;
                }
            }
        }
    }

    false
}

fn match_n(arr: &[i32], val: i32, n: usize) -> bool {
    /// ARR is a corresponding (inductive) slice.
    ///
    /// TOTAL is the sum we're searching for.
    ///
    /// N is the number of (remaining) values we need to find.
    ///
    /// SUM is the sum we've accumulated so far.
    ///
    /// Note that this function will print the resulting numbers in reverse as it walks
    /// back up the call stack.
    fn helper(arr: &[i32], total: i32, n: usize, sum: i32) -> bool {
        if n == 0 {
            if sum == total {
                print!("match_n: ");
                return true;
            }

            return false;
        }

        for idx in 0..arr.len() {
            if helper(&arr[idx + 1..], total, n - 1, sum + arr[idx]) {
                print!("{} ", arr[idx]);
                return true;
            }
        }

        false
    }

    if helper(arr, val, n, 0) {
        println!();
        true
    } else {
        false
    }
}

/// Part 2
mod throttled_func_mod {
    use std::{
        mem::MaybeUninit,
        sync::{atomic::AtomicBool, Arc, Mutex},
        time::{Duration, Instant},
    };

    /// Our global list of calls, accessible from multiple threads.
    static mut CALLS: MaybeUninit<Arc<Mutex<Vec<Instant>>>> = MaybeUninit::uninit();

    /// Remove all calls that happened more than 1 second ago. This is suboptimal because
    /// we traverse the entire vec while we know that it's sorted in terms of monotonic
    /// time. But anyways, this is not the point of this exercise. The vec of calls should
    /// be easily replaceable with a circular buffer which is more fitting to the problem
    /// at hand.
    fn cleanup(calls: &mut Vec<Instant>, current_time: Instant) {
        *calls = calls
            .iter()
            .filter(|&&time| current_time.duration_since(time) < Duration::from_secs(1))
            .copied()
            .collect();
    }

    /// Must run at most 10 times per second.
    ///
    /// This is private and cannot be accessed directly from outside the module. Instead,
    /// calling get_throttled_func() is going to ensure our globals are initialized _once_
    /// and will return a pointer to this function which can then be called safely.
    fn throttled_func() {
        let current_time = Instant::now();

        // We know that CALLS is initialized so the first unwrap() cannot fail.
        let mut calls_guard = unsafe { CALLS.as_mut_ptr().as_mut() }
            .unwrap()
            .lock()
            .unwrap();

        cleanup(&mut calls_guard, current_time);

        // The remaining calls are the ones that happened within the past second from
        // current_time.
        if calls_guard.len() >= 10 {
            println!("Not going to execute");
            return;
        }

        calls_guard.push(current_time);
        println!("Going to execute");
    }

    pub fn get_throttled_func() -> fn() {
        // The first caller is going to initialize our CALLS
        // array/vec/circular_buffer/whatever.
        static INITIALIZED: AtomicBool = AtomicBool::new(false);
        if !INITIALIZED.swap(true, std::sync::atomic::Ordering::AcqRel) {
            unsafe { CALLS.write(Arc::new(Mutex::new(Vec::new()))) };
        }

        throttled_func
    }
}

fn main() {
    let arr = [1, 2, 3, 4, 5];
    let arr2 = [-1, -2, 2, 4, 5, 0];

    println!("Part 1");
    // arr
    println!("result(11) = {}", match_three(&arr, 11));
    println!("result(13) = {}", match_three(&arr, 13));
    println!("result(0) = {}", match_three(&arr, 0));
    // arr2
    println!("result(0) = {}", match_three(&arr2, 0));

    println!("Part 3");
    // arr
    assert_eq!(match_three(&arr, 11), match_n(&arr, 11, 3));
    println!("---");
    assert_eq!(match_three(&arr, 13), match_n(&arr, 13, 3));
    println!("---");
    assert_eq!(match_three(&arr, 0), match_n(&arr, 0, 3));
    println!("---");
    // arr2
    assert_eq!(match_three(&arr2, 0), match_n(&arr2, 0, 3));
    println!("---");

    // We should get 10 "Going to execute" and 5 "Not going to execute".
    for _ in 0..15 {
        get_throttled_func()()
    }

    // We wait for 1 second to "reset" our timer.
    sleep(Duration::from_secs(1));

    // We should get again 10 "Going to execute" and 5 "Not going to execute".
    for _ in 0..15 {
        get_throttled_func()()
    }
}
