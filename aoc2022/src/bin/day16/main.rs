use std::{
    cell::RefCell,
    cmp::max,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use tools::*;

#[derive(Debug, Clone)]
struct Node {
    name: String,
    flow: i32,
    to: HashSet<String>,
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
        .parse::<i32>()
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

fn helper(
    this: &Node,
    step_left: i32,
    already_opened: HashSet<String>,
    table: &HashMap<String, Node>,
) -> i32 {
    if step_left <= 0 {
        return 0;
    }

    dbg!(this);
    dbg!(&step_left);

    if already_opened.contains(&this.name) {
        return this
            .to
            .iter()
            .map(|name| {
                helper(
                    table.get(name).unwrap(),
                    step_left - 1,
                    already_opened.clone(),
                    table,
                )
            })
            .max()
            .unwrap();
    } else {
        let x = this
            .to
            .iter()
            .map(|name| {
                helper(
                    table.get(name).unwrap(),
                    step_left - 1,
                    already_opened.clone(),
                    table,
                )
            })
            .max()
            .unwrap();
        let y = this
            .to
            .iter()
            .map(|name| {
                helper(
                    table.get(name).unwrap(),
                    step_left - 2,
                    {
                        let mut set_clone = already_opened.clone();
                        set_clone.insert(this.name.clone());
                        set_clone
                    },
                    table,
                )
            })
            .max()
            .unwrap()
            + this.flow * (step_left - 1);

        return max(x, y);
    }
}

fn day16(inputs: &Vec<String>) -> i32 {
    let nodes = parse_input(inputs);
    let mut set = HashSet::new();
    let table: HashMap<String, Node> = nodes.iter().map(|n| (n.name.clone(), n.clone())).collect();

    set.insert("AA".to_string());

    helper(table.get("AA").unwrap(), 3, set, &table)
}

fn main() {
    let input = read_file_by_line("./inputs/day16_demo.input");
    //println!("{:?}", parse_input(&input));
    println!("{}", day16(&input));

    //let input = read_file_by_line("./inputs/day16.input");
    //println!("{}", day16(&input, 1));
    //println!("{}", day16(&input, 2));
}
