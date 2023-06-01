use std::{
    cell::RefCell,
    cmp::max,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use itertools::{Itertools, ProcessResults};
use tools::*;

#[derive(Debug, Clone, PartialEq)]
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

fn helper_bkp(
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
                helper_bkp(
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
                helper_bkp(
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
                helper_bkp(
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

fn day16_bkp(inputs: &Vec<String>) -> i32 {
    let nodes = parse_input(inputs);
    //let mut set = HashSet::new();
    let table: HashMap<String, Node> = nodes.iter().map(|n| (n.name.clone(), n.clone())).collect();

    //set.insert("AA".to_string());

    let mut states_keeper = HashMap::new();
    let mut max = 0;

    for i in 1..=30 {
        let mut set = HashSet::new();
        set.insert("AA".to_string());
        max = helper_bkp(table.get("AA").unwrap(), i, set, &table, &mut states_keeper);
    }

    let mut set = HashSet::new();
    set.insert("AA".to_string());
    max = helper_bkp(
        table.get("AA").unwrap(),
        30,
        set,
        &table,
        &mut states_keeper,
    );

    max
}

fn helper(
    this: &Node,
    step_left: i32,
    table: &HashMap<String, Node>,
    pending_valves: HashSet<String>,
    states_keeper: &mut HashMap<State, i32>, // (step_left, this.name, pending_valves)
    valve_distances: &HashMap<String, HashMap<String, Option<i32>>>,
) -> i32 {
    let mut result = 0;
    let mut pv = pending_valves
        .iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    pv.sort();

    let state = State {
        step_left,
        name: this.name.clone(),
        // here, it isn;t the already open, it is the left of valves haven't open
        // I just laxy to change the status struct
        already_opened: pv,
    };

    if states_keeper.contains_key(&state) {
        return *states_keeper.get(&state).unwrap();
    }

    for next_pv in pending_valves.clone() {
        let steps = valve_distances
            .get(&this.name)
            .unwrap()
            .get(&next_pv)
            .unwrap();

        // 1 more step to open the valve
        let next_remaining_steps = step_left - steps.unwrap() - 1;

        if next_remaining_steps > 0 {
            let mut next_pending_valvas = pending_valves.clone();
            next_pending_valvas.remove(&next_pv);

            let next_result = helper(
                table.get(&next_pv).unwrap(),
                next_remaining_steps,
                table,
                next_pending_valvas,
                states_keeper,
                valve_distances,
            );

            let current_result =
                next_result + table.get(&next_pv).unwrap().flow * next_remaining_steps;

            result = result.max(current_result);
        }
    }

    states_keeper.insert(state, result);

    result
}

fn day16(inputs: &Vec<String>) -> i32 {
    let nodes = parse_input(inputs);

    let mut g = Graph::new();
    for n in &nodes {
        for t in n.to.iter() {
            g.insert(n.name.clone(), t.to_string(), 1)
        }
    }

    let mut fw = FloydWarshall::new();
    let valve_distances = fw.run(&g);
    //dbg!(&valve_distances);

    let table: HashMap<String, Node> = nodes.iter().map(|n| (n.name.clone(), n.clone())).collect();

    let mut set = HashSet::new();
    table.iter().for_each(|(k, v)| {
        if v.flow > 0 {
            set.insert(k.clone());
        }
    });
    // 	table.keys().for_each(|k| {
    //     set.insert(k.clone());
    // });
    //set.insert("AA".to_string());

    let mut states_keeper = HashMap::new();

    helper(
        table.get("AA").unwrap(),
        30,
        &table,
        set,
        &mut states_keeper,
        &valve_distances,
    )

    //max
}

fn dat16_part2(inputs: &Vec<String>) -> i32 {
    let nodes = parse_input(inputs);

    let mut g = Graph::new();
    for n in &nodes {
        for t in n.to.iter() {
            g.insert(n.name.clone(), t.to_string(), 1)
        }
    }

    let mut fw = FloydWarshall::new();
    let valve_distances = fw.run(&g);
    //dbg!(&valve_distances);

    let table: HashMap<String, Node> = nodes.iter().map(|n| (n.name.clone(), n.clone())).collect();
    let mut set = HashSet::new();
    table.iter().for_each(|(k, v)| {
        if v.flow > 0 {
            set.insert(k.clone());
        }
    });

    let mut states_keeper = HashMap::new();
    // here

    partitions(table.clone())
        .iter()
        .map(|(left, right)| {
            let left = helper(
                &left[0].1,
                26,
                &table,
                set.clone(),
                &mut states_keeper,
                &valve_distances,
            );
            let right = match right.get(0) {
                Some(rr) => helper(
                    &rr.1,
                    26,
                    &table,
                    set.clone(),
                    &mut states_keeper,
                    &valve_distances,
                ),
                None => 0,
            };
            //let right = ;
            left + right
        })
        .max()
        .unwrap()
}

fn partitions(table: HashMap<String, Node>) -> Vec<(Vec<(String, Node)>, Vec<(String, Node)>)> {
    let mut values = table.into_iter().collect::<Vec<_>>();
    let mut result: Vec<Vec<(String, Node)>> = vec![vec![values[0].clone()]];

    for value in &values[1..] {
        let mut new_result = result.clone();
        for r in result {
            let mut cloned = r.clone();
            cloned.push(value.clone());
            new_result.push(cloned);
        }
        result = new_result;
    }

    result
        .into_iter()
        .map(|left| {
            let values_cloned = values.clone();
            let right = values_cloned
                .into_iter()
                .filter(|x| !left.contains(x))
                .collect_vec();
            (
                //left.into_iter().collect::<HashMap<String, Node>>(),
                //right.into_iter().collect::<HashMap<String, Node>>(),
                left, right,
            )
        })
        .collect_vec()
}

fn main() {
    let input = read_file_by_line("./inputs/day16_demo.input");
    //let input = read_file_by_line("./inputs/day16.input");
    //println!("{:?}", parse_input(&input));
    println!("{}", day16(&input));
    println!("{}", dat16_part2(&input));
    //println!("{}", day16_bkp(&input));

    //let input = read_file_by_line("./inputs/day16.input");
    //println!("{}", day16(&input, 1));
    //println!("{}", day16(&input, 2));
}
