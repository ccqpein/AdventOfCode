use std::collections::HashMap;

use regex::Regex;
use tools::*;

fn find_ind(time: i32, len: i32) -> i32 {
    let a = time % (2 * (len - 1));
    if a < len { a } else { (2 * (len - 1)) - a }
}

fn parse_input(inputs: &[String]) -> impl Iterator<Item = (i32, i32)> {
    let re = Regex::new(r"^(\d+): (\d+)$").unwrap();
    inputs.into_iter().map(move |l| {
        let Some(c) = re.captures(l) else { panic!() };

        (c[1].parse::<i32>().unwrap(), c[2].parse::<i32>().unwrap())
    })
}

fn day13(inputs: &[String]) -> i32 {
    let table = parse_input(inputs).collect::<HashMap<_, _>>();
    let mut re = 0;
    for time in 0..=*table.keys().max().unwrap_or(&0) {
        if let Some(len) = table.get(&time) {
            if find_ind(time, *len) == 0 {
                re += time * len;
            }
        }
    }
    re
}

fn day13_2(inputs: &[String]) -> i32 {
    let table = parse_input(inputs).collect::<HashMap<_, _>>();
    let mut delay = 1;
    let max = *table.keys().max().unwrap_or(&0);
    'a: loop {
        for time in 0..=max {
            if let Some(len) = table.get(&time) {
                if find_ind(time + delay, *len) == 0 {
                    delay += 1;
                    continue 'a;
                }
            }
        }
        break;
    }
    delay
}

fn main() {
    let input = read_file_by_line("../inputs/day13.input");
    //dbg!(parse_input(&vec!["40: 12".to_string()]));

    //dbg!(find_ind(5, 4));
    //dbg!(find_ind(4, 4));
    //dbg!(find_ind(6, 4));
    //dbg!(find_ind(7, 4));
    //dbg!(find_ind(0, 3));
    //dbg!(find_ind(1, 3));

    dbg!(day13(&input));
    dbg!(day13_2(&input));
}
