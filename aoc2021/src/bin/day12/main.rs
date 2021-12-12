use std::collections::{HashMap, HashSet};

use tools::*;

fn part1(input: &Vec<String>) -> i32 {
    let mut graph_table: HashMap<String, HashSet<String>> = HashMap::new();

    input.iter().for_each(|l| {
        let a = l.split('-').map(|s| s.to_string()).collect::<Vec<String>>();
        let b = graph_table.entry(a[0].clone()).or_insert(HashSet::new());
        b.insert(a[1].clone());

        let b = graph_table.entry(a[1].clone()).or_insert(HashSet::new());
        b.insert(a[0].clone());
    });

    graph_table["start"]
        .iter()
        .map(|next| {
            helper(
                &graph_table,
                ["start".to_string()].into_iter().collect(),
                next.to_string(),
            )
        })
        .sum()
}

fn is_uppercase(s: &String) -> bool {
    s.chars().all(|c| c.is_uppercase())
}

fn helper(
    relation_table: &HashMap<String, HashSet<String>>,
    already: HashSet<String>,
    this: String,
) -> i32 {
    if &this == "end" {
        return 1;
    }

    let all_next = relation_table[&this].clone();

    all_next
        .iter()
        .map(|next| {
            if already.contains(next) {
                0
            } else {
                helper(
                    relation_table,
                    if !is_uppercase(&this) {
                        let mut a = already.clone();
                        a.insert(this.clone());
                        a
                    } else {
                        already.clone()
                    },
                    next.to_string(),
                )
            }
        })
        .sum()
}

fn part2(input: &Vec<String>) -> i32 {
    let mut graph_table: HashMap<String, HashSet<String>> = HashMap::new();

    input.iter().for_each(|l| {
        let a = l.split('-').map(|s| s.to_string()).collect::<Vec<String>>();
        let b = graph_table.entry(a[0].clone()).or_insert(HashSet::new());
        b.insert(a[1].clone());

        let b = graph_table.entry(a[1].clone()).or_insert(HashSet::new());
        b.insert(a[0].clone());
    });

    graph_table["start"]
        .iter()
        .map(|next| {
            helper2(
                &graph_table,
                ["start".to_string()].into_iter().collect(),
                HashMap::new(),
                true,
                next.to_string(),
            )
        })
        .sum()
}

fn helper2(
    relation_table: &HashMap<String, HashSet<String>>,
    already: HashSet<String>,
    times_count: HashMap<String, usize>,
    flag: bool,
    this: String,
) -> i32 {
    if &this == "end" {
        return 1;
    }

    let all_next = relation_table[&this].clone();

    all_next
        .iter()
        .map(|next| {
            let mut next_times_count = times_count.clone();
            let mut next_flag = flag;
            let next_set = if !is_uppercase(&this) {
                let mut a = already.clone();
                let this_time = next_times_count.entry(this.clone()).or_insert(0);
                if next_flag {
                    if *this_time == 1 {
                        // already came once
                        next_times_count.keys().for_each(|k| {
                            a.insert(k.to_string());
                        });
                        next_flag = false
                    } else {
                        *this_time += 1;
                    }
                } else {
                    a.insert(this.clone());
                }

                a
            } else {
                already.clone()
            };

            if next_set.contains(next) {
                0
            } else {
                helper2(
                    relation_table,
                    next_set,
                    next_times_count,
                    next_flag,
                    next.to_string(),
                )
            }
        })
        .sum()
}

fn main() {
    let input = read_file_by_line("./src/bin/day12/day12.input");
    //let input = read_file_by_line("./src/bin/day12/day12_demo.input");
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
