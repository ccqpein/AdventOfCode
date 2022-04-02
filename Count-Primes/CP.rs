use std::collections::HashSet;

pub fn count_primes(n: i32) -> i32 {
    let mut count = 0;
    let mut set = HashSet::new();
    for i in 2..n {
        if !set.contains(&i) {
            count += 1;
            (2..=(n / i)).into_iter().for_each(|a| {
                set.insert(a * i);
            })
        }
    }
    count
}

fn main() {
    dbg!(count_primes(10));
    dbg!(count_primes(1));
    dbg!(count_primes(10000));
    dbg!(count_primes(5000000));
}
