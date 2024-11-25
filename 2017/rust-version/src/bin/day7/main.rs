use regex::*;
use std::{
    cell::{LazyCell, RefCell},
    collections::HashMap,
    rc::Rc,
    sync::LazyLock,
};
use tools::*;

#[derive(Clone)]
struct Tree {
    value: String,
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
            value: table.get(key).unwrap().to_string(),
            nodes: Some(RefCell::new(cc)),
        }
    } else {
        Tree {
            value: table.get(key).unwrap().to_string(),
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
                    dbg!(child);
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
    for (_, children) in &table_children {
        for child in children {
            make_node(&child, &table, &table_children, &mut cache);
        }
    }
    cache.get(&top).unwrap().clone()
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

fn main() {
    let input = read_file_by_line("../inputs/day7.input");
    //let input = read_file_by_line("../inputs/day7_demo.input");

    // dbg!(regex_input(
    //     "uyxvzt (1046) -> uemmhd, ctdjc, gxqkboz, nvudb, dadueoc"
    // ));
    // dbg!(regex_input("ancik (61)"));

    dbg!(day7(&input));
}
