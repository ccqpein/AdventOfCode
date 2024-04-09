use std::collections::{HashMap, HashSet};

use rand;
use rand::seq::SliceRandom;
use regex::Regex;
use tools::*;

fn day19(input: &[String]) -> usize {
    let mut sp = input.split(|l| l == "");

    let mut table = HashMap::new();
    let re0 = Regex::new(r#"^(.+) => (.+)$"#).unwrap();
    sp.next().unwrap().into_iter().for_each(|l| {
        let (_, [k, v]) = re0.captures(l).unwrap().extract();
        let en = table.entry(k).or_insert(vec![]);
        en.push(v)
    });

    //dbg!(&table);

    let input = sp.next().unwrap();

    //dbg!(&input[0]);

    p1(&table, &input[0])
}

fn p1(table: &HashMap<&str, Vec<&str>>, input: &str) -> usize {
    let mut set = HashSet::new();
    for (re, vs) in table {
        handle1(*re, input, vs, &mut set)
    }
    set.len()
}

fn handle1(re: &str, input: &str, replaces: &[&str], set: &mut HashSet<String>) {
    let re = Regex::new(re).unwrap();
    // dbg!(re
    //     .find_iter(input)
    //     .map(|m| (m.start(), m.end()))
    //     .collect::<Vec<_>>());

    for r in re.find_iter(input) {
        for rr in replaces {
            //dbg!(input[0..r.start()].to_string() + rr + &input[r.end()..]);
            let new_s = input[0..r.start()].to_string() + rr + &input[r.end()..];
            set.insert(new_s);
        }
    }
}

fn day19_2(input: &[String]) -> usize {
    let mut sp = input.split(|l| l == "");

    let mut table = HashMap::new();
    let re0 = Regex::new(r#"^(.+) => (.+)$"#).unwrap();
    sp.next().unwrap().into_iter().for_each(|l| {
        let (_, [k, v]) = re0.captures(l).unwrap().extract();
        table.insert(v, k);
    });

    //dbg!(table.len());

    let input = sp.next().unwrap();
    handle2(&input[0], &table).unwrap()
}

fn handle2(input: &str, replaces: &HashMap<&str, &str>) -> Option<usize> {
    //println!("input: {}", input);
    if input == "e" {
        return Some(0);
    }

    replaces
        .iter()
        .filter_map(|(k, v)| {
            let re = Regex::new(k).unwrap();
            re.find_iter(input)
                .filter_map(|r| {
                    if let Some(d) = handle2(
                        &(input[0..r.start()].to_string() + v + &input[r.end()..]),
                        replaces,
                    ) {
                        Some(1 + d)
                    } else {
                        None
                    }
                })
                .min()
        })
        .min()
}

fn day19_22(input: &[String]) -> usize {
    let mut sp = input.split(|l| l == "");

    let mut table = HashMap::new();
    let re0 = Regex::new(r#"^(.+) => (.+)$"#).unwrap();
    sp.next().unwrap().into_iter().for_each(|l| {
        let (_, [k, v]) = re0.captures(l).unwrap().extract();
        let en = table.entry(k).or_insert(vec![]);
        en.push(v)
    });

    //dbg!(table.len());

    let input = sp.next().unwrap();
    handle22(&input[0], "e", &table).unwrap()
}

fn handle22(target: &str, input: &str, replaces: &HashMap<&str, Vec<&str>>) -> Option<usize> {
    if input.len() > target.len() {
        return None;
    }

    if input == target {
        return Some(0);
    }

    replaces
        .iter()
        .filter_map(|(k, v)| {
            let re = Regex::new(k).unwrap();
            re.find_iter(input)
                .filter_map(|r| {
                    v.iter()
                        .filter_map(|vv| {
                            if let Some(d) = handle22(
                                target,
                                &(input[0..r.start()].to_string() + vv + &input[r.end()..]),
                                replaces,
                            ) {
                                Some(1 + d)
                            } else {
                                None
                            }
                        })
                        .min()
                })
                .min()
        })
        .min()
}

fn day19_23(input: &[String]) -> usize {
    let mut sp = input.split(|l| l == "");

    //let mut table = HashMap::new();
    let mut pairs = vec![];
    let re0 = Regex::new(r#"^(.+) => (.+)$"#).unwrap();
    sp.next().unwrap().into_iter().for_each(|l| {
        let (_, [k, v]) = re0.captures(l).unwrap().extract();
        pairs.push((k, v, Regex::new(v).unwrap()))
    });

    //dbg!(table.len());

    let input = sp.next().unwrap();
    let mut s = input[0].clone();
    let mut count = 0;
    let mut rng = rand::thread_rng();
    while s.len() > 1 {
        let start = s.clone();
        for (k, v, re) in pairs.iter() {
            if s.contains(v) {
                count += re.find_iter(&s).count();
                s = s.replace(v, k);
            }
        }

        if s == start {
            pairs.shuffle(&mut rng);
            s = input[0].clone();
            count = 0;
        }
    }
    count
}

fn main() {
    let input = read_file_by_line("../inputs/day19.input");

    //handle1("H", "HOH", &vec!["HO", "OH"], &mut HashSet::new());
    println!("1: {:?}", day19(&input)); // 576

    // dbg!(handle2(
    //     "HOHOHO",
    //     &[
    //         ("H", "e"),
    //         ("O", "e"),
    //         ("HO", "H"),
    //         ("OH", "H"),
    //         ("HH", "O"),
    //     ]
    //     .into_iter()
    //     .collect::<HashMap<_, _>>(),
    // ));

    // dbg!(handle22(
    //     "HOHOHO",
    //     "e",
    //     &[
    //         ("e", vec!["H", "O"]),
    //         ("H", vec!["HO", "OH"]),
    //         ("O", vec!["HH"]),
    //     ]
    //     .into_iter()
    //     .collect::<HashMap<_, _>>(),
    // ));
    //println!("2: {:?}", day19_22(&input));
    println!("2: {:?}", day19_23(&input)); // 207
}
