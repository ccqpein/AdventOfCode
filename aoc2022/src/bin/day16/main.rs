use std::{cell::RefCell, rc::Rc};

use tools::*;

#[derive(Debug)]
struct Node {
    name: String,
    flow: usize,
    to: Vec<String>,
}

fn parse_line(line: &str) -> Node {
    let mut ss = line.split(' ');
    let name = ss.nth(1).unwrap().to_string();
    let flow = ss
        .nth(2)
        .unwrap()
        .split('=')
        .nth(1)
        .unwrap()
        .trim_end_matches(';')
        .parse::<usize>()
        .unwrap();
    ss.nth(3);
    let valves = ss.map(|v| v.trim_end_matches(',').to_string()).collect();

    Node {
        name,
        flow,
        to: valves,
    }
}

fn parse_input(input: &[String]) -> Vec<Node> {
    input.iter().map(|line| parse_line(line)).collect()
}

//fn day16(inputs: V)

fn main() {
    let input = read_file_by_line("./inputs/day16_demo.input");
    println!("{:?}", parse_input(&input));
    //let input = read_file_by_line("./inputs/day16.input");
    //println!("{}", day16(&input, 1));
    //println!("{}", day16(&input, 2));
}
