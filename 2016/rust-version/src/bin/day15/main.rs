use regex::Regex;
use tools::*;

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

// part1 and part2
fn part1(inputs: &[Input]) -> usize {
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
    let mut parsed_input = parse_input(&input);
    println!("part1: {}", part1(&parsed_input));

    parsed_input.push(Input {
        num: parsed_input.last().unwrap().num + 1,
        positions: 11,
        at_position: 0,
    });
    println!("part2: {}", part1(&parsed_input));
}
