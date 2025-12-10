use std::collections::HashSet;

use itertools::Itertools;
use rayon::prelude::*;
use regex::*;
use tools::*;

fn parse_input(input: &[String]) -> Vec<(Vec<Vec<usize>>, Vec<i64>)> {
    let re = Regex::new(
        r"^\[.*\]\s*(?P<tuples>(?:\(\d+(?:,\d+)*\)\s*)+)\s*\{\s*(?P<list>\d+(?:,\d+)*)\s*\}$",
    )
    .unwrap();

    let tuple_content_re = Regex::new(r"\((\d+(?:,\d+)*)\)").unwrap();
    let mut res = vec![];
    for line in input {
        match re.captures(line) {
            Some(captures) => {
                let tuples_str = captures.name("tuples").unwrap().as_str();
                let list_str = captures.name("list").unwrap().as_str();

                let mut parsed_tuples: Vec<Vec<usize>> = Vec::new();
                for t_match in tuple_content_re.captures_iter(tuples_str) {
                    let inner_str = t_match.get(1).unwrap().as_str(); // The content inside the parentheses
                    let inner_vec: Vec<usize> = inner_str
                        .split(',')
                        .filter_map(|s| s.trim().parse::<usize>().ok())
                        .collect();
                    parsed_tuples.push(inner_vec);
                }

                let parsed_list: Vec<i64> = list_str
                    .split(',')
                    .filter_map(|s| s.trim().parse::<i64>().ok())
                    .collect();
                res.push((parsed_tuples, parsed_list));
            }
            None => {
                println!("??: {line}")
            }
        }
    }

    res
}

fn press_time_limit(button: &[usize], target: &[i64]) -> usize {
    button.iter().map(|ind| target[*ind]).min().unwrap() as usize
}

fn cal_new_target(button: &[usize], times: usize, target: &[i64]) -> Vec<i64> {
    let mut nt = target.to_vec();
    button.iter().for_each(|ind| nt[*ind] -= times as i64);
    nt
}

fn has_all_button(buttons: &[Vec<usize>], target: &[i64]) -> bool {
    let bbs = buttons
        .iter()
        .flatten()
        .map(|u| *u)
        .collect::<HashSet<usize>>();
    //dbg!(&bbs);
    target
        .iter()
        .enumerate()
        .filter_map(|(ind, n)| if *n > 0 { Some(ind) } else { None })
        .all(|tt| bbs.contains(&tt))
}

fn one_entry(buttons: &[Vec<usize>], target: &[i64]) -> Option<usize> {
    if buttons.len() == 0 {
        return None;
    }
    //dbg!((press_time_limit(buttons.get(0).unwrap(), target)));
    for t in (0..=(press_time_limit(buttons.get(0).unwrap(), target))).rev() {
        let new_target = cal_new_target(buttons.get(0).unwrap(), t, target);
        // dbg!(buttons.get(0));
        // dbg!(&new_target);
        // dbg!(has_all_button(&buttons[1..], &new_target));

        if has_all_button(&buttons[1..], &new_target) && new_target.iter().all(|n| *n >= 0) {
            if new_target.iter().all(|n| *n == 0) {
                return Some(t);
            } else {
                match one_entry(&buttons[1..], &new_target) {
                    Some(n) => return Some(t + n),
                    None => (),
                }
            }
        }
    }
    None
}

fn day10_2(input: &[String]) -> usize {
    let mut input = parse_input(input);

    //dbg!(input);
    // let mut res = 0usize;
    // for (mut buttons, target) in input {
    //     buttons.sort_by(|a, b| b.len().cmp(&a.len()));
    //     // dbg!(&buttons);
    //     // dbg!(&target);
    //     //dbg!(one_entry(&buttons, &target));
    //     res += one_entry(&buttons, &target).unwrap()
    // }
    // res

    let sum = input
        .par_iter_mut()
        .map(|(buttons, target)| {
            buttons.sort_by(|a, b| b.len().cmp(&a.len()));
            let x = one_entry(&buttons, &target).unwrap();
            println!("get result: {x}");
            x
        })
        .sum();
    sum
}

fn main() {
    let input = read_file_by_line("../inputs/day10.input");
    //let input = read_file_by_line("../inputs/day10_demo.input");
    //dbg!(day4(&input));
    //dbg!(day10_2(&input[0..1]));
    dbg!(day10_2(&input));

    // dbg!(one_entry(
    //     &vec![
    //         vec![3],
    //         vec![1, 3],
    //         vec![2],
    //         vec![2, 3],
    //         vec![0, 2],
    //         vec![0, 1],
    //     ],
    //     &vec![0, 1, 0, 2],
    // ));
}
