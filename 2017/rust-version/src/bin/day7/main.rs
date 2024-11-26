use regex::*;
use std::{
    cell::{LazyCell, RefCell},
    collections::HashMap,
};
use tools::*;

#[derive(Clone, Debug)]
struct Tree {
    value: i32,
    sum: i32,
    nodes: Option<RefCell<Vec<Tree>>>,
}

fn regex_input(input: &str) -> (String, String, Option<String>) {
    const RE: LazyCell<Regex> =
        LazyCell::new(|| Regex::new(r#"^(\w+)\s+\((\d+)\)(\s+->\s+([\w,\s]+))?"#).unwrap());

    if let Some(a) = RE.captures(input) {
        return (
            a[1].to_string(),
            a[2].to_string(),
            a.get(4).map_or(None, |m| Some(m.as_str().to_string())),
        );
    }

    (String::new(), String::new(), None)
}

fn make_node(
    key: &String,
    table: &HashMap<String, i32>,
    table_children: &HashMap<String, Vec<String>>,
    cache: &mut HashMap<String, Tree>,
) -> Tree {
    if let Some(c) = cache.get(key) {
        return c.clone();
    }

    let tree = if let Some(children) = table_children.get(key) {
        let mut cc = vec![];
        for child in children {
            cc.push(make_node(child, table, table_children, cache))
        }

        Tree {
            value: *table.get(key).unwrap(),
            sum: *table.get(key).unwrap() + cc.iter().map(|node| node.sum).sum::<i32>(),
            nodes: Some(RefCell::new(cc)),
        }
    } else {
        Tree {
            value: *table.get(key).unwrap(),
            sum: *table.get(key).unwrap(),
            nodes: None,
        }
    };

    cache.insert(key.to_string(), tree.clone());
    tree
}

fn make_tree(inputs: &[String], top: String) -> Tree {
    let mut table = HashMap::new();
    let mut table_children = HashMap::new();

    for line in inputs {
        match regex_input(line) {
            (name, number, Some(list)) => {
                for child in list.split(", ") {
                    //dbg!(child);
                    table_children
                        .entry(name.clone())
                        .or_insert(vec![])
                        .push(child.to_string());
                }
                table.insert(name, number.parse::<i32>().unwrap());
            }
            (name, number, None) => {
                table.insert(name, number.parse::<i32>().unwrap());
            }
        }
    }

    //:= make tree
    let mut cache = HashMap::new();
    for (p, children) in &table_children {
        make_node(&p, &table, &table_children, &mut cache);
    }
    cache.get(&top).unwrap().clone()
}

fn sum_the_tree(t: &Tree) -> i32 {
    t.value
        + match &t.nodes {
            Some(c) => c.borrow().iter().map(|tt| sum_the_tree(tt)).sum(),
            None => 0,
        }
}

fn day7(inputs: &[String]) -> String {
    let mut children_to_parent = HashMap::new();

    for line in inputs {
        match regex_input(line) {
            (name, number, Some(list)) => {
                for child in list.split(", ") {
                    //dbg!(child);
                    children_to_parent.insert(child.to_string(), name.clone());
                }
            }
            (name, number, None) => {}
        }
    }
    let mut c = children_to_parent.keys().next().unwrap();

    while let Some(p) = children_to_parent.get(c) {
        c = p
    }

    c.clone()
}

// I cannot believe I need write this function
fn find_the_diff_ele(tt: &[Tree]) -> Option<i32> {
    let mut count_map = HashMap::new();

    for n in tt {
        *count_map.entry(n.sum).or_insert(0) += 1;
    }

    for n in tt {
        if *count_map.get(&n.sum).unwrap() == 1 {
            return Some(n.sum);
        }
    }

    None
}

fn dig_tree(t: &Tree, other_sum: i32) -> i32 {
    let diff_v = find_the_diff_ele(&t.nodes.as_ref().unwrap().borrow());
    //dbg!(t);
    //dbg!(diff_v);
    if diff_v.is_none() {
        return t.value - (t.sum - other_sum);
    }

    // find which tree
    let next_t_ind = t
        .nodes
        .as_ref()
        .unwrap()
        .borrow()
        .iter()
        .position(|n| n.sum == diff_v.clone().unwrap())
        .unwrap();

    //dbg!(next_t_ind);
    let next_t = t.nodes.as_ref().unwrap().borrow();

    let next_t = next_t.get(next_t_ind).unwrap();

    let other_v = t
        .nodes
        .as_ref()
        .unwrap()
        .borrow()
        .iter()
        .find(|n| n.sum != diff_v.unwrap())
        .unwrap()
        .sum;

    dig_tree(next_t, other_v)
}

fn day7_2(inputs: &[String], top: String) -> i32 {
    let tree = make_tree(inputs, top);
    dig_tree(&tree, 0)
}

fn main() {
    let input = read_file_by_line("../inputs/day7.input");
    //let input = read_file_by_line("../inputs/day7_demo.input");

    // dbg!(regex_input(
    //     "uyxvzt (1046) -> uemmhd, ctdjc, gxqkboz, nvudb, dadueoc"
    // ));
    // dbg!(regex_input("ancik (61)"));

    let top = day7(&input);
    let tt = make_tree(&input, top);
    //dbg!(&tt);
    dbg!(dig_tree(&tt, 0));
}
