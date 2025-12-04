use std::collections::{HashMap, VecDeque};

//use cl_format::*;
use itertools::Itertools;
use tools::*;

fn parse_input(input: &[String]) -> Map<char> {
    let m = Map::from(
        input
            .iter()
            .map(|l| l.chars().collect::<Vec<char>>())
            .collect::<Vec<_>>(),
    );

    m
}

fn day4(input: &[String]) -> usize {
    let m = parse_input(input);
    let mut res = 0;
    let mut cleaned_coops = vec![];
    for r in 0..m.row_len() {
        for c in 0..m.col_len() {
            //dbg!(m.get_around((r, c)).collect::<Vec<_>>());
            if m.get((r, c)) == Some(&'@') {
                match m.get_around((r, c)).counts_by(|(coop, c)| c).get(&'@') {
                    Some(n) if *n < 4 => {
                        cleaned_coops.push((r, c));
                        res += 1;
                    }
                    None => res += 1,
                    _ => (),
                }
            }
        }
    }
    //cl_format!("(~{~{~a~^ ~}~})", &cleaned_coops);
    // print!("(");
    // for (r, c) in cleaned_coops {
    //     print!("({r} {c})")
    // }
    // println!(")");
    res
}

fn day4_2(input: &[String]) -> usize {
    let mut m = parse_input(input);
    let mut res = 0;
    let mut cleaned_coops = vec![(0, 0)];

    while cleaned_coops.len() != 0 {
        cleaned_coops.clear();
        for r in 0..m.row_len() {
            for c in 0..m.col_len() {
                //dbg!(m.get_around((r, c)).collect::<Vec<_>>());
                if m.get((r, c)) == Some(&'@') {
                    match m.get_around((r, c)).counts_by(|(coop, c)| c).get(&'@') {
                        Some(n) if *n < 4 => {
                            cleaned_coops.push((r, c));
                            res += 1;
                        }
                        None => {
                            cleaned_coops.push((r, c));
                            res += 1;
                        }
                        _ => (),
                    }
                }
            }
        }

        for (r, c) in cleaned_coops.iter() {
            m.set(*r, *c, '.').unwrap();
        }
    }

    res
}

fn main() {
    let input = read_file_by_line("../inputs/day4.input");
    //let input = read_file_by_line("../inputs/day4_demo.input");
    dbg!(day4(&input));
    dbg!(day4_2(&input));
}
