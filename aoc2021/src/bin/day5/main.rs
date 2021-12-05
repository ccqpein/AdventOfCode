use std::{collections::HashMap, io::BufRead};

use tools::*;

fn parse_line(s: &str) -> Vec<Vec<usize>> {
    s.split(" -> ")
        .map(|ss| {
            ss.split(',')
                .into_iter()
                .map(|s| s.parse::<usize>().unwrap())
                .collect()
        })
        .collect::<Vec<Vec<usize>>>()
}

fn part1(input: &Vec<String>) -> usize {
    let mut whole_map: HashMap<(usize, usize), usize> = HashMap::new();
    for l in input {
        let ll = parse_line(l);
        match (ll[0].as_slice(), ll[1].as_slice()) {
            ([a, b], [c, d]) => {
                if a == c {
                    if b >= d {
                        for y in (*d..=*b) {
                            *whole_map.entry((*a, y)).or_insert(0) += 1
                        }
                    } else {
                        for y in (*b..=*d) {
                            *whole_map.entry((*a, y)).or_insert(0) += 1
                        }
                    }
                } else if b == d {
                    if a >= c {
                        for x in (*c..=*a) {
                            *whole_map.entry((x, *b)).or_insert(0) += 1
                        }
                    } else {
                        for x in (*a..=*c) {
                            *whole_map.entry((x, *b)).or_insert(0) += 1
                        }
                    }
                }
            }
            _ => {
                panic!()
            }
        }
    }
    let mut result = 0;
    for (k, v) in whole_map {
        if v >= 2 {
            result += 1;
        }
    }
    result
}

fn part2(input: &Vec<String>) -> i32 {
    let mut whole_map: HashMap<(i32, i32), i32> = HashMap::new();
    for l in input {
        let ll = parse_line(l);
        match (ll[0].as_slice(), ll[1].as_slice()) {
            ([a, b], [c, d]) => {
                if a == c {
                    if b >= d {
                        for y in (*d..=*b) {
                            *whole_map.entry((*a as i32, y as i32)).or_insert(0) += 1
                        }
                    } else {
                        for y in (*b..=*d) {
                            *whole_map.entry((*a as i32, y as i32)).or_insert(0) += 1
                        }
                    }
                } else if b == d {
                    if a >= c {
                        for x in (*c..=*a) {
                            *whole_map.entry((x as i32, *b as i32)).or_insert(0) += 1
                        }
                    } else {
                        for x in (*a..=*c) {
                            *whole_map.entry((x as i32, *b as i32)).or_insert(0) += 1
                        }
                    }
                } else if (*a as i32 - *c as i32).abs() == (*b as i32 - *d as i32).abs() {
                    let x_offset = if (*a as i32) < (*c as i32) { 1 } else { -1 };
                    let y_offset = if (*b as i32) < (*d as i32) { 1 } else { -1 };

                    for step in 0..=(*a as i32 - *c as i32).abs() as usize {
                        *whole_map
                            .entry((
                                *a as i32 + step as i32 * x_offset,
                                *b as i32 + step as i32 * y_offset,
                            ))
                            .or_insert(0) += 1
                    }
                }
            }
            _ => {
                panic!()
            }
        }
    }
    let mut result = 0;
    for (k, v) in whole_map {
        if v >= 2 {
            result += 1;
        }
    }
    result
}

fn main() {
    let input = read_file_by_line("./src/bin/day5/day5.input");
    //let input = read_file_by_line("./src/bin/day5/day5_demo.input");
    //println!("{}", part1(&input));
    println!("{}", part2(&input));
}
