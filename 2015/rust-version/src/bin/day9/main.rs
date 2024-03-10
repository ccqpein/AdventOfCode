use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Add;

use regex::*;
use tools::*;

fn helper<ID, V>(
    g: &Graph<ID, V>,
    this: ID,
    visited: HashSet<ID>,
    part2: bool,
) -> <V as Add>::Output
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone + Add<Output = V> + Default,
{
    let temp = g
        .get(&this)
        .unwrap()
        .iter()
        .filter(|i| !visited.contains(i.id()))
        .map(|i| {
            let mut vvv = visited.clone();
            vvv.insert(this.clone());
            i.v().clone() + helper(g, i.id().clone(), vvv, part2)
        });
    if part2 {
        temp.max().unwrap_or(<V as Default>::default())
    } else {
        temp.min().unwrap_or(<V as Default>::default())
    }
}

fn day9(input: &[String]) -> usize {
    let re = Regex::new(r"^(.+) to (.+) = (\d+)$").unwrap();
    let mut g = Graph::new(GraphType::Undirected);
    for l in input {
        if let Some(cap) = re.captures(l) {
            let (_, [a, b, dis]) = cap.extract();
            g.insert(a, b, dis.parse::<usize>().unwrap());
        }
    }

    g.all_ids()
        .map(|i| helper(&g, i, HashSet::new(), false))
        .min()
        .unwrap()
}

fn day9_2(input: &[String]) -> usize {
    let re = Regex::new(r"^(.+) to (.+) = (\d+)$").unwrap();
    let mut g = Graph::new(GraphType::Undirected);
    for l in input {
        if let Some(cap) = re.captures(l) {
            let (_, [a, b, dis]) = cap.extract();
            g.insert(a, b, dis.parse::<usize>().unwrap());
        }
    }

    g.all_ids()
        .map(|i| helper(&g, i, HashSet::new(), true))
        .max()
        .unwrap()
}

fn main() {
    let input = read_file_by_line("../inputs/day9.input");
    // let input = vec![
    //     "London to Dublin = 464".to_string(),
    //     "London to Belfast = 518".to_string(),
    //     "Dublin to Belfast = 141".to_string(),
    // ];
    println!("1: {:?}", day9(&input));
    println!("2: {:?}", day9_2(&input));
}
