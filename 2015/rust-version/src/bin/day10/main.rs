use itertools::*;
use tools::*;

fn helper(input: &str) -> String {
    let mut input = input.chars();
    let mut last = input.next().unwrap();
    let mut len = 1;
    let mut bucket = vec![];

    for c in input {
        if c == last {
            len += 1;
        } else {
            bucket.push((len, last));
            last = c;
            len = 1;
        }
    }

    if len != 0 {
        bucket.push((len, last));
    }

    let acc = bucket.into_iter().fold(vec![], |mut acc, x| {
        acc.append(&mut vec![x.0.to_string(), x.1.to_string()]);
        acc
    });

    String::from_iter(acc.into_iter())
}

fn day10(mut input: String, time: usize) -> String {
    for _ in 0..time {
        input = helper(&input)
    }

    input
}

fn main() {
    println!("1: {:?}", day10("1".to_string(), 5));
    println!("1: {:?}", day10("1113122113".to_string(), 40).len()); // 360154
    println!("2: {:?}", day10("1113122113".to_string(), 50).len()); // 5103798
}
