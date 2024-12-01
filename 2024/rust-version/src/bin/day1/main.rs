use std::{collections::HashMap, iter::zip};

use tools::*;

fn day1(inputs: &[String]) -> i32 {
    let mut left = vec![];
    let mut right = vec![];
    inputs.into_iter().for_each(|l| {
        let mut ss = l
            .split(' ')
            .filter(|&c| c != "")
            .map(|a| a.parse::<i32>().unwrap());
        left.push(ss.next().unwrap());
        right.push(ss.next().unwrap());
    });

    left.sort();
    right.sort();

    zip(left, right).map(|(a, b)| (a - b).abs()).sum()
}

fn day1_2(inputs: &[String]) -> i32 {
    let mut left = vec![];
    let mut right = HashMap::new();
    inputs.into_iter().for_each(|l| {
        let mut ss = l
            .split(' ')
            .filter(|&c| c != "")
            .map(|a| a.parse::<i32>().unwrap());
        left.push(ss.next().unwrap());
        *right.entry(ss.next().unwrap()).or_insert(0) += 1;
    });

    left.into_iter()
        .map(|a| right.get(&a).unwrap_or(&0) * a)
        .sum()
}

fn main() {
    let input = read_file_by_line("../inputs/day1.input");
    dbg!(day1(&input));
    dbg!(day1_2(&input));
}
