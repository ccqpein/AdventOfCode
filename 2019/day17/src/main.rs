use day11::*;
use std::collections::HashSet;

fn day17(filepath: &str) {
    use std::env;

    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();

    //make intcode
    let mut intcode = read_the_damn_intcode(&format!("{}/src/{}", path, filepath));
    intcode.append(&mut [0; 10000].to_vec()); // give the buffer

    let mut ic = Intcode::new(&intcode);
    ic.run(None); // run intcode machine

    //println!("{:?}", ic.output.clone());
    // ic.output = vec![
    //     46, 46, 35, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 10, 46, 46, 35, 46, 46, 46, 46, 46, 46,
    //     46, 46, 46, 46, 10, 35, 35, 35, 35, 35, 35, 35, 46, 46, 46, 35, 35, 35, 10, 35, 46, 35, 46,
    //     46, 46, 35, 46, 46, 46, 35, 46, 35, 10, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
    //     10, 46, 46, 35, 46, 46, 46, 35, 46, 46, 46, 35, 46, 46, 10, 46, 46, 35, 35, 35, 35, 35, 46,
    //     46, 46, 46, 46, 46, 10,
    // ];
    print_map(ic.output.clone());

    let (mut row, mut col) = (0, 0);
    let mut hash_set: HashSet<(i64, i64)> = HashSet::new();

    // update hashset
    for c in ic.output {
        if c == 10 {
            row += 1;
            col = 0;
            continue;
        }

        if c == 35 || c == 94 {
            hash_set.insert((row, col));
        }
        col += 1;
    }

    let mut sum = 0;
    for r in 0..=50 {
        for c in 0..=50 {
            if find_if_intersection(&hash_set, (r, c)) {
                sum += r * c;
            }
        }
    }
    println!("{}", sum);
}

fn print_map(mut output: Vec<i64>) {
    use std::io::{self, BufRead};
    use std::str;

    let content = output.iter_mut().map(|x| *x as u8).collect::<Vec<u8>>();

    let strings = str::from_utf8(&content).unwrap().lines();
    let mut count = 1;
    for s in strings {
        println!("{}", s);
        count += 1;
    }
}

fn find_if_intersection(set: &HashSet<(i64, i64)>, coop: (i64, i64)) -> bool {
    if set.get(&coop).is_none() {
        return false;
    }
    let a = (coop.0, coop.1 + 1);
    let b = (coop.0, coop.1 - 1);
    let c = (coop.0 + 1, coop.1);
    let d = (coop.0 - 1, coop.1);
    match (set.get(&a), set.get(&b), set.get(&c), set.get(&d)) {
        (Some(_), Some(_), Some(_), Some(_)) => true,
        _ => false,
    }
}

fn main() {
    day17("day17.input")
}
