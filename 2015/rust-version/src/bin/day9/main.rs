use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Add;

use regex::*;
use tools::*;

fn helper<ID, V>(g: &Graph<ID, V>, this: ID, visited: HashSet<ID>) -> <V as Add>::Output
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone + Add<Output = V> + Default,
{
    let mut min_v = vec![];
    for i in g.get(&this).unwrap() {
        if visited.contains(i.id()) {
            continue;
        }
        let mut vvv = visited.clone();
        vvv.insert(this.clone());
        min_v.push(i.v().clone() + helper(g, i.id().clone(), vvv));
    }

    min_v
        .iter()
        .min()
        .unwrap_or(&<V as Default>::default())
        .clone()
}

fn day9(input: &[String]) {
    let re = Regex::new(r"^(.+) to (.+) = (\d+)$").unwrap();
    let mut g = Graph::new(GraphType::Undirected);
    for l in input {
        if let Some(cap) = re.captures(l) {
            let (_, [a, b, dis]) = cap.extract();
            g.insert(a, b, dis.parse::<usize>().unwrap());
        }
    }

    dbg!(helper(&g, "London", HashSet::new()));
}

fn main() {
    let input = read_file_by_line("../inputs/day9.input");
    let input = vec![
        "London to Dublin = 464".to_string(),
        "London to Belfast = 518".to_string(),
        "Dublin to Belfast = 141".to_string(),
    ];
    day9(&input);
}
