fn fib(n: int) -> int{
    match n {
        0..1 => 1,
        _ => fib(n-1) + fib(n-2)
    }
}

fn fact(n: int) -> int {
    let mut x = 0;
    for i in range(0, n) {
        x += i
    }
    return x
}

fn main() {
    println!("fact of {:d} is {:d}.", 10, fact(10));
    println!("fib of {:d} is {:d}.", 10, fib(10));
}
