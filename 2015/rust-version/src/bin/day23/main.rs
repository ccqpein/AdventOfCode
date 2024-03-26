use std::collections::HashMap;

use regex::*;
use tools::*;

fn op<'s, 'h: 's>(
    comm: (&str, Match<'h>, Option<Match<'h>>),
    table: &mut HashMap<&'s str, i64>,
) -> Option<i64> {
    match comm.0 {
        "hlf" => {
            let en = table.entry(comm.1.as_str()).or_insert(0);
            *en = *en / 2;
        }
        "tpl" => {
            let en = table.entry(comm.1.as_str()).or_insert(0);
            *en = *en * 3;
        }
        "inc" => {
            let en = table.entry(comm.1.as_str()).or_insert(0);
            *en += 1;
        }
        "jmp" => return Some(comm.1.as_str().parse::<i64>().unwrap()),
        "jie" => {
            let en = table.entry(comm.1.as_str()).or_insert(0);
            if *en % 2 == 0 {
                return Some(comm.2.unwrap().as_str()[2..].parse::<i64>().unwrap());
            }
        }
        "jio" => {
            let en = table.entry(comm.1.as_str()).or_insert(0);
            if *en == 1 {
                return Some(comm.2.unwrap().as_str()[2..].parse::<i64>().unwrap());
            }
        }
        _ => unreachable!(),
    }
    None
}

fn day23(input: &[String], p2: bool) -> HashMap<&str, i64> {
    let mut table = HashMap::new();
    if p2 {
        table.insert("a", 1);
    }

    let re = Regex::new(r#"(.{3}) (\w|[+-]\d+)(, [+-]\d+)?"#).unwrap();
    let mut i = 0_i64;
    while (i as usize) < input.len() {
        let l = &input[i as usize];
        //dbg!(re.captures(l));
        match re.captures(l).unwrap().iter().collect::<Vec<_>>()[..] {
            [_, Some(a), Some(b), Some(c)] => {
                if let Some(of) = op((a.as_str(), b, Some(c)), &mut table) {
                    i += of;
                    continue;
                }
            }
            [_, Some(a), Some(b), None] => {
                if let Some(of) = op((a.as_str(), b, None), &mut table) {
                    i += of;
                    continue;
                }
            }
            _ => (),
        }

        i += 1
    }

    table
}

fn main() {
    let input = read_file_by_line("../inputs/day23.input");
    //let input = read_file_by_line("../inputs/day23_demo.input");
    //println!("1: {:?}", day23(&input).get("a"));
    println!("1: {:?}", day23(&input, false).get("b"));

    println!("2: {:?}", day23(&input, true).get("b"));
}
