use std::collections::HashSet;

use regex::Regex;
use tools::*;

fn add_both_nodes(g: &Graph<&str, i32>, a: &str, b: &str) -> i32 {
    g.get(&a)
        .unwrap()
        .iter()
        .filter(|x| *x.id() == b)
        .next()
        .unwrap()
        .v()
        + g.get(&b)
            .unwrap()
            .iter()
            .filter(|x| *x.id() == a)
            .next()
            .unwrap()
            .v()
}

fn helper(g: &Graph<&str, i32>, start: &str, visited: &mut Vec<String>) -> i32 {
    g.get(&start)
        .unwrap()
        .iter()
        .filter_map(|id_pair| {
            if !visited.contains(&id_pair.id().to_string()) {
                let mut sum = 0;
                visited.push(start.to_string());
                sum += id_pair.v()
                    + g.get(id_pair.id())
                        .unwrap()
                        .iter()
                        .filter(|id_pair: &&IDValuePiar<&str, i32>| *id_pair.id() == start)
                        .next()
                        .unwrap()
                        .v();
                sum += helper(g, id_pair.id(), visited);
                visited.pop();

                Some(sum)
            } else {
                None
            }
        })
        .max()
        .unwrap_or_else(|| add_both_nodes(g, start, &visited[0]))
}

fn day13(input: &[String]) -> i32 {
    let re =
        Regex::new(r"^(.+) would (.+) (\d+) happiness units by sitting next to (.+).$").unwrap();

    let mut g = Graph::new(GraphType::Directed);
    for line in input {
        let (_, [x, g_or_l, h, y]) = re.captures(line).unwrap().extract();
        match g_or_l {
            "gain" => g.insert(x, y, h.parse::<i32>().unwrap()),
            "lose" => g.insert(x, y, -h.parse::<i32>().unwrap()),
            _ => unreachable!(),
        }
    }
    let start = g.all_ids().next().unwrap();
    let mut set = Vec::new();
    helper(&g, start, &mut set)
}

fn day13_2(input: &[String]) -> i32 {
    let re =
        Regex::new(r"^(.+) would (.+) (\d+) happiness units by sitting next to (.+).$").unwrap();

    let mut g = Graph::new(GraphType::Directed);
    for line in input {
        let (_, [x, g_or_l, h, y]) = re.captures(line).unwrap().extract();
        match g_or_l {
            "gain" => g.insert(x, y, h.parse::<i32>().unwrap()),
            "lose" => g.insert(x, y, -h.parse::<i32>().unwrap()),
            _ => unreachable!(),
        }
    }

    for id in g.all_ids().cloned().collect::<Vec<_>>() {
        g.insert("me", id, 0);
        g.insert(id, "me", 0)
    }

    let start = g.all_ids().next().unwrap();
    let mut set = Vec::new();
    //dbg!(&g);
    helper(&g, start, &mut set)
}

fn main() {
    let input = read_file_by_line("../inputs/day13.input");
    //let input = read_file_by_line("../inputs/day13_demo.input");

    println!("1: {:?}", day13(&input));
    println!("2: {:?}", day13_2(&input));
}
