use std::collections::HashMap;

use tools::*;

fn parse_line(line: &str) -> Vec<String> {
    line.split(' ').map(|s| s.to_string()).collect::<Vec<_>>()
}

fn day8(inputs: &[String]) -> (i32, i32) {
    let mut table = HashMap::new();
    let mut max_v = 0;

    for line in inputs {
        let ll = parse_line(line);

        let bb = match ll[5].as_str() {
            "<=" => {
                let v = table.entry(ll[4].clone()).or_insert(0);
                if *v <= ll[6].parse::<i32>().unwrap() {
                    true
                } else {
                    false
                }
            }
            "==" => {
                let v = table.entry(ll[4].clone()).or_insert(0);
                if *v == ll[6].parse::<i32>().unwrap() {
                    true
                } else {
                    false
                }
            }
            "<" => {
                let v = table.entry(ll[4].clone()).or_insert(0);
                if *v < ll[6].parse::<i32>().unwrap() {
                    true
                } else {
                    false
                }
            }
            ">" => {
                let v = table.entry(ll[4].clone()).or_insert(0);
                if *v > ll[6].parse::<i32>().unwrap() {
                    true
                } else {
                    false
                }
            }
            "!=" => {
                let v = table.entry(ll[4].clone()).or_insert(0);
                if *v != ll[6].parse::<i32>().unwrap() {
                    true
                } else {
                    false
                }
            }
            ">=" => {
                let v = table.entry(ll[4].clone()).or_insert(0);
                if *v >= ll[6].parse::<i32>().unwrap() {
                    true
                } else {
                    false
                }
            }
            x @ _ => {
                dbg!(x);
                panic!()
            }
        };

        if bb {
            match ll[1].as_str() {
                "inc" => {
                    let v = table.entry(ll[0].clone()).or_insert(0);
                    *v += ll[2].parse::<i32>().unwrap();
                    if *v >= max_v {
                        max_v = *v
                    }
                }
                "dec" => {
                    let v = table.entry(ll[0].clone()).or_insert(0);
                    *v -= ll[2].parse::<i32>().unwrap();
                    if *v >= max_v {
                        max_v = *v
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    (*table.values().max().unwrap(), max_v)
}

fn main() {
    let input = read_file_by_line("../inputs/day8.input");
    //dbg!(parse_line(&input[0]));
    dbg!(day8(&input));
}
