#![feature(is_sorted)]
use std::{
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use tools::*;

use regex::Regex;

fn parse_line(s: &str) -> Vec<&str> {
    s.split("-").collect()
}

fn group_chars(table: &HashMap<char, i32>) -> Vec<Vec<char>> {
    let mut vs = table
        .values()
        .cloned()
        .collect::<HashSet<i32>>()
        .into_iter()
        .collect::<Vec<i32>>();

    vs.sort_by(|a, b| b.partial_cmp(a).unwrap());

    let mut res = vec![];

    for n in vs {
        res.push(
            table
                .iter()
                .filter(|(_, v)| **v == n)
                .map(|(c, _)| *c)
                .collect(),
        );
    }
    res
}

fn check_checksum(bucket: &Vec<Vec<char>>, b: &str) -> bool {
    let mut index = 0;
    for c in b.chars() {
        if index < bucket.len() - 1 {
            if bucket[index + 1].contains(&c) {
                index += 1
            }
        }

        if !bucket[index].contains(&c) {
            return false;
        }
    }
    true
}

fn day4(inputs: &[String]) -> i64 {
    let re = Regex::new(r"(\d+)\[(\w+)\]").unwrap();
    let mut res = 0;

    for line in inputs {
        let ll = parse_line(line);
        let (_, [a, b]) = re
            .captures(ll.last().unwrap())
            .map(|c| c.extract())
            .unwrap();
        //println!("{a}, {b}");

        let mut table = HashMap::new();
        ll[0..ll.len() - 1].iter().for_each(|l| {
            l.chars().for_each(|c| {
                let en = table.entry(c).or_insert(0);
                *en += 1;
            })
        });
        //dbg!(&table);
        if b.chars().any(|c| table.get(&c).is_none()) {
            continue;
        }

        let bucket = group_chars(&table);
        if check_checksum(&bucket, b) {
            res += a.parse::<i64>().unwrap();
        }
    }
    res
}

static ALPHABET: LazyLock<HashMap<usize, char>> = LazyLock::new(|| -> HashMap<usize, char> {
    let mut table = HashMap::new();
    for (ind, c) in [
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    ]
    .into_iter()
    .enumerate()
    {
        table.insert(ind, c);
    }
    table
});

fn char_rotate(c: char, rotate: i64) -> char {
    let offset = rotate % 26;

    let mut init = *ALPHABET.iter().find(|(_, v)| **v == c).unwrap().0;

    init = (init + offset as usize) % 26;
    *ALPHABET.get(&init).unwrap()
}

fn line_rotate(l: &[&str], rotate: i64) -> Vec<String> {
    l.iter()
        .map(|l| {
            l.chars()
                .map(|c| char_rotate(c, rotate))
                .collect::<String>()
        })
        .collect()
}

fn day4_2(inputs: &[String]) -> i64 {
    let re = Regex::new(r"(\d+)\[(\w+)\]").unwrap();

    for line in inputs {
        let ll = parse_line(line);
        let (_, [a, b]) = re
            .captures(ll.last().unwrap())
            .map(|c| c.extract())
            .unwrap();
        //println!("{a}, {b}");
        let aa = a.parse::<i64>().unwrap();
        //println!("{}", line_rotate(&ll[0..ll.len() - 1], aa).join(" "));
        if line_rotate(&ll[0..ll.len() - 1], aa).contains(&"northpole".to_string()) {
            return aa;
        }
    }
    0
}

fn main() {
    let input = read_file_by_line("../inputs/day4.input");
    //let input = read_file_by_line("../inputs/day4_demo.input");
    println!("part1: {}", day4(&input));
    println!("part2: {}", day4_2(&input));
}
