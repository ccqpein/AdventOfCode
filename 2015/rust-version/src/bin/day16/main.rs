use std::collections::HashMap;

use regex::Regex;
use tools::*;

fn p1(l: &str, check_list_table: &HashMap<&str, usize>, re: &Regex) -> Option<String> {
    let (_, [sue, item0, num0, item1, num1, item2, num2]) = re.captures(l).unwrap().extract();
    if *check_list_table.get(item0).unwrap() == num0.parse().unwrap()
        && *check_list_table.get(item1).unwrap() == num1.parse().unwrap()
        && *check_list_table.get(item2).unwrap() == num2.parse().unwrap()
    {
        return Some(sue.to_string());
    }
    None
}

fn p2(l: &str, check_list_table: &HashMap<&str, usize>, re: &Regex) -> Option<String> {
    let (_, [sue, item0, num0, item1, num1, item2, num2]) = re.captures(l).unwrap().extract();
    for (item, num) in [(item0, num0), (item1, num1), (item2, num2)] {
        match item {
            "cats" | "trees" => {
                if *check_list_table.get(item).unwrap() >= num.parse().unwrap() {
                    return None;
                }
            }
            "pomeranians" | "goldfish" => {
                if *check_list_table.get(item).unwrap() <= num.parse().unwrap() {
                    return None;
                }
            }
            _ => {
                if *check_list_table.get(item).unwrap() != num.parse().unwrap() {
                    return None;
                }
            }
        }
    }

    Some(sue.to_string())
}

fn day16(input: &[String], part1: bool) -> Option<String> {
    let check_list_re = Regex::new(r#"(.+): (\d+)"#).unwrap();
    let check_list_table = r#"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"#
        .lines()
        .map(|l| {
            let (_, [item, num]) = check_list_re.captures(l).unwrap().extract();
            (item, num.parse::<usize>().unwrap())
        })
        .collect::<HashMap<_, _>>();

    let re = Regex::new(r#"^Sue (\d+): (.+): (\d+), (.+): (\d+), (.+): (\d+)$"#).unwrap();

    for l in input {
        if part1 {
            if let Some(s) = p1(l, &check_list_table, &re) {
                return Some(s);
            }
        } else {
            if let Some(s) = p2(l, &check_list_table, &re) {
                return Some(s);
            }
        }
    }
    None
}

fn main() {
    let input = read_file_by_line("../inputs/day16.input");
    println!("1: {:?}", day16(&input, true));
    println!("2: {:?}", day16(&input, false));
}
