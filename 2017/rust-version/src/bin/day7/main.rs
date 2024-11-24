use regex::*;
use std::{
    cell::{LazyCell, RefCell},
    rc::Rc,
    sync::LazyLock,
};
use tools::*;

struct Tree {
    value: String,
    nodes: Rc<RefCell<Vec<Tree>>>,
}

fn regex_input(input: &str) -> (String, String, Option<String>) {
    const re: LazyCell<Regex> =
        LazyCell::new(|| Regex::new(r#"^(\w+)\s+\((\d+)\)(\s+->\s+([\w,\s]+))?"#).unwrap());

    // if let Some((_, [name, weight, list])) = re.captures(input).map(|c| c.extract()) {
    //     return (name.to_string(), weight.to_string(), list.to_string());
    // }

    if let Some(a) = re.captures(input) {
        return (
            a[1].to_string(),
            a[2].to_string(),
            a.get(4).map_or(None, |m| Some(m.as_str().to_string())),
        );
    }

    (String::new(), String::new(), None)
}

fn make_tree(inputs: &[String]) -> Tree {
    let mut table = HashMap::new();

    for line in inputs {
        match regex_input(line) {
            (name, number, Some(list)) => {
                for child in list.split(", ") {
                    //:= todo
                }
            }
            (name, number, None) => table.insert(name, number.parse::<i32>().unwrap()),
        }
    }
}

fn main() {
    let input = read_file_by_line("../inputs/day7.input");

    dbg!(regex_input(
        "uyxvzt (1046) -> uemmhd, ctdjc, gxqkboz, nvudb, dadueoc"
    ));
    dbg!(regex_input("ancik (61)"));
}
