use std::collections::HashMap;

use tools::{read_file_by_line, vec_of_sum_rest};

fn parser(ls: &Vec<String>) -> Vec<u64> {
    ls.iter().map(|s| s.parse::<u64>().unwrap()).collect()
}

fn sort_between(a: &Vec<u64>, i: usize, j: usize, buffer: &mut Vec<u64>) {
    buffer.clear();
    buffer.extend_from_slice(&a[i..j]);
    buffer.sort();
}

fn find_sum(buffer: &Vec<u64>, a: u64) -> bool {
    for j in (0..buffer.len()).rev() {
        for i in 0..j {
            if buffer[j] + buffer[i] > a {
                break;
            }
            if buffer[j] + buffer[i] == a {
                return true;
            }
        }
    }
    false
}

fn part1(input: &Vec<String>) -> u64 {
    let a = parser(input);
    let mut buffer = vec![];
    for ind in 25..a.len() {
        sort_between(&a, ind - 25, ind, &mut buffer);
        if !find_sum(&buffer, a[ind]) {
            return a[ind];
        }
    }
    0
}

fn part2(input: &Vec<String>) -> (u64, u64) {
    let a = parser(input);
    let mut buffer = vec![];
    let x = part1(input);

    for le in 2..a.len() {
        for start in 0..a.len() - le {
            sort_between(&a, start, start + le, &mut buffer);
            if buffer.iter().sum::<u64>() == x {
                return (buffer[0], buffer[le - 1]);
            }
        }
    }
    (0, 0)
}

fn part2_10times(a: Vec<u64>, x: u64) -> (u64, u64) {
    let mut buffer = vec![];

    for le in 2..a.len() {
        for start in 0..a.len() - le {
            sort_between(&a, start, start + le, &mut buffer);
            if buffer.iter().sum::<u64>() == x {
                return (buffer[0], buffer[le - 1]);
            }
        }
    }
    (0, 0)
}

fn part2_v2(a: Vec<u64>, x: u64) -> (u64, u64) {
    //use std::ops::Add;
    let mut buffer: Vec<u128> = vec![0];
    let _ = a.iter().rev().fold(0, |acc, n| {
        buffer.push(acc + *n as u128);
        acc + *n as u128
    });

    buffer = buffer.split_last().unwrap().1.into();
    buffer.reverse(); // sync with a

    let mut table: HashMap<&u128, Vec<usize>> = HashMap::new();
    buffer.iter().enumerate().for_each(|(i, d)| {
        let a = table.entry(&d).or_insert(vec![]);
        a.push(i)
    });

    for ind in 1..a.len() {
        match table.get(&&(buffer[ind] + x as u128)) {
            Some(ii) => {
                if let Some(is) = ii.iter().filter(|&&d| d < ind).next() {
                    let mut aa = a[*is..ind + 1].to_vec();
                    aa.sort();
                    return (aa[0 as usize], aa[ind - is]);
                }
            }
            None => {}
        };
    }
    (0, 0)
}

fn main() {
    use std::time::Instant;
    let input = read_file_by_line(String::from("./src/day9.input"));
    println!("{:?}", part1(&input));
    let r2 = part2(&input);
    println!("{:?} -> {:?}", r2, r2.0 + r2.1);

    // test
    // let now = Instant::now();
    // for _ in 0..10 {
    //     part2_10times(parser(&input), part1(&input));
    // }
    // println!("10 times {}", now.elapsed().as_millis());

    // let now = Instant::now();
    // for _ in 0..10 {
    //     println!("{:?}", part2_v2(parser(&input), part1(&input)));
    // }
    // println!("10 times {}", now.elapsed().as_millis());

    use std::ops::Add;
    let a: Vec<u32> = vec![1, 2, 3, 4, 5];
    let aa = vec_of_sum_rest(&a, <u32 as Add<_>>::add);
    dbg!(aa);
}
