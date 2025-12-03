use std::collections::{HashMap, VecDeque};

use tools::*;

fn find_the_largest(
    input: &[usize],
    left: usize,
    cache: &mut HashMap<(Vec<usize>, usize), VecDeque<usize>>,
) -> VecDeque<usize> {
    if let Some(aa) = cache.get(&(input.to_vec(), left)) {
        return aa.clone();
    }
    if input.len() < left {
        return vec![].into();
    }
    if left == 1 {
        return vec![*input.iter().max().unwrap()].into();
    }
    if input.len() == left {
        return input.to_vec().into();
    }

    let mut res = vec![];

    for key in (1..=9).rev() {
        for next_round in find_key_start(input, key) {
            let mut x = find_the_largest(&next_round[1..], left - 1, cache);
            if x.is_empty() {
                continue;
            }
            x.push_front(key);
            res.push(x)
        }
    }

    let aa = res.get(0).unwrap().clone();
    cache.insert((input.to_vec(), left), aa.clone());
    aa
}

fn find_key_start(sl: &[usize], key: usize) -> Vec<&[usize]> {
    let mut aa = vec![];
    for (ind, v) in sl.iter().enumerate() {
        if *v == key {
            aa.push(ind);
        }
    }

    let mut res = vec![];
    for x in aa {
        res.push(sl.get(x..).unwrap())
    }

    res
}

fn string_to_digit_vec(s: &str) -> Vec<usize> {
    let mut digits = Vec::new();
    for c in s.chars() {
        if let Some(digit) = c.to_digit(10) {
            digits.push(digit as usize);
        }
    }
    digits
}

fn vecdeque_to_usize(deque: &VecDeque<usize>) -> usize {
    let mut result: usize = 0;
    for &digit in deque.iter() {
        result = result * 10 + digit;
    }
    result
}

fn day3(input: &[String], left: usize) -> usize {
    let mut res = 0;
    let mut cache = HashMap::new();
    for line in input {
        let x = string_to_digit_vec(line);
        //dbg!(&x);
        //dbg!(find_the_largest(&x, left));
        res += vecdeque_to_usize(&find_the_largest(&x, left, &mut cache))
    }

    res
}

fn main() {
    let input = read_file_by_line("../inputs/day3.input");
    //let input = read_file_by_line("../inputs/day3_demo.input");

    dbg!(day3(&input, 2));
    dbg!(day3(&input, 12));
}
