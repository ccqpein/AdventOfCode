use core::panic;
use std::{collections::HashMap, mem};

use regex::Regex;
use tools::*;

fn parse_inputs(inputs: &[String]) -> Vec<(String, String, Option<String>)> {
    let re = Regex::new(r"^(\w+)\s+(\d+|[a-z])(\s+(-?\d+|[a-z]))?$").unwrap();

    let mut parsed_input = vec![];
    for l in inputs {
        dbg!(l);
        let Some(c) = re.captures(l) else { panic!() };
        parsed_input.push((
            c[1].to_string(),
            c[2].to_string(),
            c.get(4).map(|v| v.as_str().to_string()),
        ));
    }

    //dbg!(parsed_input);
    parsed_input
}

fn day18(inputs: &[String]) {
    let parsed_input = parse_inputs(inputs);
    let mut ind = 0;
    let mut memory = HashMap::new();
    let mut last = 0;
    loop {
        let Some((a, b, c)) = parsed_input.get(ind as usize) else {
            break;
        };
        println!("{:?}", memory);
        println!("{a} {b} {:?}", c);

        match (a.as_str(), b.as_str(), c.as_ref().map(|v| v.as_str())) {
            ("snd", _, None) => {
                last = *memory.entry(b).or_insert(0);
            }
            ("set", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                *memory.entry(b).or_insert(0) = digt
            }
            ("add", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                // match memory.get_mut(b) {
                //     Some(bb) => *bb += digt,
                //     None => (),
                // }

                *memory.entry(b).or_insert(0) += digt
            }
            ("mul", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                // match memory.get_mut(b) {
                //     Some(bb) => *bb *= digt,
                //     None => (),
                // }

                *memory.entry(b).or_insert(0) *= digt
            }
            ("mod", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                // match memory.get_mut(b) {
                //     Some(bb) => *bb %= digt,
                //     None => (),
                // }

                *memory.entry(b).or_insert(0) %= digt;
            }
            ("rcv", _, _) => {
                let en = memory.entry(b).or_insert(0);
                if *en != 0 {
                    break;
                }
            }
            ("jgz", _, _) => {
                // match memory.get_mut(b) {
                //     Some(bb) => {
                //         if *bb > 0 {
                //             ind += c.as_ref().unwrap().parse::<i64>().unwrap();
                //             continue;
                //         }
                //     }
                //     None => (),
                // }

                let digt = match b.parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(b).or_insert(0),
                };

                //let en = memory.entry(b).or_insert(0);
                if digt > 0 {
                    ind += c.as_ref().unwrap().parse::<i64>().unwrap();
                    continue;
                }
            }
            _ => unreachable!(),
        }

        ind += 1
    }
    println!("table: {:?}", memory);
    println!("last: {last}")
}

fn main() {
    let input = read_file_by_line("../inputs/day18.input");
    //let input = read_file_by_line("../inputs/day18_demo.input");
    //parse_inputs(&input);

    day18(&input);
}
