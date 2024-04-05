use std::collections::HashSet;

use itertools::Itertools;
use tools::*;

fn day3(input: &[String]) -> usize {
    let input = input[0].clone();
    let mut set = HashSet::new();
    let (mut x, mut y) = (0, 0);

    set.insert((x, y));
    for c in input.chars() {
        match c {
            '^' => y += 1,
            '>' => x += 1,
            '<' => x -= 1,
            'v' => y -= 1,
            _ => unreachable!(),
        }

        set.insert((x, y));
    }

    set.len()
}

fn day3_part2(input: &[String]) -> usize {
    let input = input[0].clone();
    let mut set = HashSet::new();
    let (mut x0, mut y0) = (0, 0);

    let (mut x1, mut y1) = (0, 0);

    set.insert((x0, y0));

    for mut c in &input.chars().chunks(2) {
        match c.next().unwrap() {
            '^' => y0 += 1,
            '>' => x0 += 1,
            '<' => x0 -= 1,
            'v' => y0 -= 1,
            _ => unreachable!(),
        }
        set.insert((x0, y0));

        match c.next().unwrap() {
            '^' => y1 += 1,
            '>' => x1 += 1,
            '<' => x1 -= 1,
            'v' => y1 -= 1,
            _ => unreachable!(),
        }
        set.insert((x1, y1));
    }

    set.len()
}

fn main() {
    let input = read_file_by_line("../inputs/day3.input");
    //let input = ["^v^v^v^v^v".to_string()];
    println!("{:?}", day3(&input)); // 2081
    println!("{:?}", day3_part2(&input)); // 2341
}
