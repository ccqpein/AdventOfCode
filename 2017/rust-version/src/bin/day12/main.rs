use std::collections::HashSet;

use regex::Regex;
use tools::*;

fn parse_line(line: &str) -> (String, Vec<i32>) {
    let re = Regex::new(r"^(\d+) <-> ([\d, ]+)$").unwrap();
    //dbg!(re.captures(line));

    let Some(c) = re.captures(line) else { panic!() };
    //dbg!(&c);

    (
        c[1].to_string(),
        c[2].split(&[',', ' '][..])
            .filter(|s| *s != "")
            .map(|c| c.parse::<i32>().unwrap())
            .collect(),
    )
}

fn parse_input(inputs: &[String]) -> tools::Graph<i32, i32> {
    let inputs = inputs.iter().map(|l| parse_line(l));
    let mut g = tools::Graph::new(tools::GraphType::Undirected);
    for (head, tails) in inputs {
        let hh = head.parse().unwrap();
        tails.into_iter().for_each(|tail| {
            g.insert(hh, tail, 1);
        });
    }

    g
}

fn day12(inputs: &[String]) -> usize {
    let g = parse_input(inputs);
    let mut visited = HashSet::new();
    visited.insert(0);
    let mut next = g
        .get(&0)
        .unwrap()
        .as_slice()
        .into_iter()
        .map(|p| p.id())
        .collect::<Vec<_>>();

    while !next.is_empty() {
        let this = next.pop().unwrap();
        if visited.contains(this) {
            continue;
        }

        next.append(
            &mut g
                .get(&this)
                .unwrap()
                .as_slice()
                .into_iter()
                .map(|p| p.id())
                .collect::<Vec<_>>(),
        );

        visited.insert(*this);
    }

    visited.len()
}

fn main() {
    let input = read_file_by_line("../inputs/day12.input");
    //let input = read_file_by_line("../inputs/day12_demo.input");

    //parse_line("2 <-> 0, 3, 4");
    //dbg!(parse_input(&vec!["2 <-> 0, 3, 4".to_string()]));
    dbg!(day12(&input));
}
