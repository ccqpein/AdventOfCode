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

#[derive(Hash, Eq, PartialEq)]
struct State {
    step_left: i32,
    name: String,
    already_opened: Vec<String>,
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
    states_keeper: &mut HashMap<State, i32>, // (step_left, this.name, already_opened)
) -> i32 {
    if step_left <= 0 {
        return 0;
    }
    let mut ao = already_opened
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    ao.sort();

    let state = State {
        step_left,
        name: this.name.clone(),
        already_opened: ao,
    };

    if states_keeper.contains_key(&state) {
        return *states_keeper.get(&state).unwrap();
    }
    //dbg!(this);
    //dbg!(&step_left);

    if already_opened.contains(&this.name) {
        let x = this
            .to
            .iter()
            .map(|name| {
                helper(
                    table.get(name).unwrap(),
                    step_left - 1,
                    already_opened.clone(),
                    table,
                    states_keeper,
                )
            })
            .max()
            .unwrap();
        states_keeper.insert(state, x);
        return x;
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
                    states_keeper,
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
                    states_keeper,
                )
            })
            .max()
            .unwrap()
            + this.flow * (step_left - 1);

        states_keeper.insert(state, max(x, y));
        return max(x, y);
    }
}

fn day16(inputs: &Vec<String>) -> i32 {
    let nodes = parse_input(inputs);
    //let mut set = HashSet::new();
    let table: HashMap<String, Node> = nodes.iter().map(|n| (n.name.clone(), n.clone())).collect();

    //set.insert("AA".to_string());

    let mut states_keeper = HashMap::new();
    let mut max = 0;

    for i in 1..=19 {
        let mut set = HashSet::new();
        set.insert("AA".to_string());
        max = helper(table.get("AA").unwrap(), i, set, &table, &mut states_keeper);
    }

    let mut set = HashSet::new();
    set.insert("AA".to_string());
    max = helper(
        table.get("AA").unwrap(),
        30,
        set,
        &table,
        &mut states_keeper,
    );

    max
}

fn main() {
    let input = read_file_by_line("./inputs/day16_demo.input");
    let input = read_file_by_line("./inputs/day16.input");
    //println!("{:?}", parse_input(&input));
    println!("{}", day16(&input));

    //let input = read_file_by_line("./inputs/day16.input");
    //println!("{}", day16(&input, 1));
    //println!("{}", day16(&input, 2));
}
