use itertools::Itertools;
use regex::Regex;
use tools::*;

// copy from internet
fn gcd(mut a: usize, mut b: usize) -> usize {
    if a == b {
        return a;
    }
    if b > a {
        let temp = a;
        a = b;
        b = temp;
    }
    while b > 0 {
        let temp = a;
        a = b;
        b = temp % b;
    }
    return a;
}

fn lcm(a: usize, b: usize) -> usize {
    return a * (b / gcd(a, b));
}
///////////

#[derive(Debug)]
struct Input {
    num: usize,
    positions: usize,
    at_position: usize,
}

fn parse_input(s: &[String]) -> Vec<Input> {
    let re = Regex::new(r"^Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).$")
        .unwrap();
    s.iter()
        .filter_map(|ss| {
            let Some((_, [num, postions, at_pos])) = re.captures(ss).map(|c| c.extract()) else {
                return None;
            };
            Some(Input {
                num: num.parse().unwrap(),
                positions: postions.parse().unwrap(),
                at_position: at_pos.parse().unwrap(),
            })
        })
        .collect()
}

fn part1(inputs: &[Input]) -> usize {
    // let lcm = inputs
    //     .iter()
    //     .map(|i| i.positions)
    //     .fold(1, |acc, x| lcm(acc, x));
    // dbg!(&lcm);
    for t in 0.. {
        if inputs
            .iter()
            .map(|input| (t + input.num + input.at_position) % input.positions)
            .all(|v| v == 0)
        {
            return t;
        }
    }
    0
}

fn main() {
    // dbg!(parse_input(
    //     vec!["Disc #1 has 13 positions; at time=0, it is at position 1.".to_string()].as_slice()
    // ));
    let input = read_file_by_line("../inputs/day15.input");
    //let input = read_file_by_line("../inputs/day15_demo.input");
    println!("part1: {}", part1(&parse_input(&input)))
}
