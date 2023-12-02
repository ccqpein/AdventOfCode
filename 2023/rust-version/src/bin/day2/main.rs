use std::collections::HashMap;

use itertools::Itertools;
use regex::Regex;
use tools::*;

fn day2(input: &[String]) -> usize {
    //let mut cache = vec![];
    //let re = Regex::new(r"\d+\s+\b\w\b").unwrap();
    let mut result = 0;
    let mut cache = HashMap::new();
    for (ind, line) in input.iter().enumerate() {
        //cache.push(line.split(&[':', ';'][..]).collect::<Vec<_>>());
        let x = line.split(&[':', ';'][..]).collect::<Vec<_>>();
        for xx in &x[1..] {
            //dbg!(re.find_iter(xx).collect::<Vec<_>>());
            // dbg!(re
            //     .find_iter(xx)
            //     .map(|m| m.as_str().parse::<i32>().unwrap())
            //      .collect::<Vec<_>>());
            let mut xxx = xx.split(&[' ', ','][..]).collect::<Vec<_>>();
            xxx.retain(|x| *x != "");

            for (n, key) in xxx.into_iter().tuples() {
                let en = cache.entry(key).or_insert(0);
                let nn = n.parse::<i32>().unwrap();
                if *en <= nn {
                    *en = nn;
                }
            }
            //dbg!(xxx);
        }
        if *cache.get("red").unwrap_or(&0) <= 12
            && *cache.get("green").unwrap_or(&0) <= 13
            && *cache.get("blue").unwrap_or(&0) <= 14
        {
            //print!("{}, ", ind + 1);
            result += ind + 1
        }

        cache.clear()
    }

    dbg!(cache);
    result
}

fn day2_2(input: &[String]) -> usize {
    let mut result = 0;
    let mut cache = HashMap::new();
    let re = Regex::new(r"(\d+)\s+([^,;]+)").unwrap();
    for (_, line) in input.iter().enumerate() {
        for (_, [num, color]) in re.captures_iter(line).map(|c| c.extract()) {
            let en = cache.entry(color).or_insert(0);
            let nn = num.parse::<usize>().unwrap();
            if *en <= nn {
                *en = nn;
            }
        }

        result += *cache.get("red").unwrap_or(&0)
            * *cache.get("green").unwrap_or(&0)
            * *cache.get("blue").unwrap_or(&0);

        cache.clear()
    }

    //dbg!(cache);
    result
}

fn main() {
    //let input = read_file_by_line("../inputs/day2_demo.input");
    let input = read_file_by_line("../inputs/day2.input");
    //println!("{:?}", input);
    //println!("{:?}", day2(&input));
    println!("{}", day2_2(&input));
}
