use std::collections::HashSet;

use tools::*;

fn day3(input: &Vec<String>) -> u32 {
    let mut all_chars = vec![];
    for line in input {
        let length = line.len();
        let (f, s) = line.split_at(length / 2);
        let a: HashSet<char> = f.chars().collect();
        let b: HashSet<char> = s.chars().collect();
        let cc: char = *a.intersection(&b).next().unwrap();
        all_chars.push(cc);
    }

    all_chars
        .iter()
        .map(|c| {
            if c.is_lowercase() {
                *c as u32 - 96
            } else {
                *c as u32 - 64 + 26
            }
        })
        .sum()
}

fn day3_part2(input: &Vec<String>) -> u32 {
    let mut all_chars = vec![];
    let ii = input
        .iter()
        .map(|line| line.chars().collect::<HashSet<char>>())
        .collect::<Vec<HashSet<char>>>();

    for i in ii.chunks(3) {
        let aa = i[0].intersection(&i[1]).cloned().collect::<HashSet<char>>();
        all_chars.push(*aa.intersection(&i[2]).next().unwrap());
    }

    all_chars
        .iter()
        .map(|c| {
            if c.is_lowercase() {
                *c as u32 - 96
            } else {
                *c as u32 - 64 + 26
            }
        })
        .sum()
}

fn main() {
    //let input = read_file_by_line("./inputs/day3_demo.input")
    let input = read_file_by_line("./inputs/day3.input");
    println!("{}", day3(&input));
    println!("{}", day3_part2(&input));
}
