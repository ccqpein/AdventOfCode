use std::{
    collections::{HashMap, HashSet},
    hash::Hasher,
};

use itertools::Itertools;
use tools::*;

fn count_comb(sorted_l: &[usize], target: usize) -> usize {
    if target == 0 {
        return 1;
    }

    let mut all = 0;

    for (ind, x) in sorted_l.iter().enumerate() {
        if *x <= target {
            all += count_comb(&sorted_l[ind + 1..], target - x)
        }
    }

    all
}

fn count_comb_2(
    sorted_l: &[usize],
    target: usize,
    deep: usize,
    set: &mut HashMap<usize, usize>,
) -> usize {
    if target == 0 {
        *set.entry(deep).or_insert(0) += 1;
        return 1;
    }

    let mut all = 0;

    for (ind, x) in sorted_l.iter().enumerate() {
        if *x <= target {
            all += count_comb_2(&sorted_l[ind + 1..], target - x, deep + 1, set)
        }
    }

    all
}

fn day17(input: &[usize], target: usize, p2: bool) -> usize {
    let mut input = input.iter().cloned().collect::<Vec<_>>();
    input.sort_by(|a, b| b.partial_cmp(a).unwrap());

    if !p2 {
        count_comb(&input, target)
    } else {
        let mut set = HashMap::new();
        count_comb_2(&input, target, 1, &mut set);
        let kk = set.keys();
        let sk = kk.sorted().next().unwrap();
        *set.get(sk).unwrap()
    }
}

fn main() {
    let input = read_file_by_line("../inputs/day17.input")
        .into_iter()
        .map(|a| a.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    //let input = vec![20, 15, 10, 5, 5];
    println!("1: {:?}", day17(&input, 150, false)); // 654
    println!("2: {:?}", day17(&input, 150, true)) // 57
}
