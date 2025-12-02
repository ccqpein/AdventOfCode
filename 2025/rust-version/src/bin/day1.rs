use regex::Regex;
use tools::*;

fn parse_input(input: &[String]) -> Vec<(String, String)> {
    let re = Regex::new(r"(\w)(\d+)").unwrap();
    // dbg!(
    //     input
    //         .iter()
    //         .map(|l| {
    //             let rr = re.captures(l).unwrap();
    //             (rr[1].to_string(), rr[2].to_string())
    //         })
    //         .collect::<Vec<_>>()
    // );

    input
        .iter()
        .map(|l| {
            let rr = re.captures(l).unwrap();
            (rr[1].to_string(), rr[2].to_string())
        })
        .collect::<Vec<_>>()
}

fn day1(input: &[String]) -> (i32, i32) {
    let input = parse_input(input);
    let mut start = 50;
    let mut p1_res = 0;
    let mut p2_res = 0;

    for (d, x) in input {
        match d.as_str() {
            "L" => {
                for _ in 1..=x.parse::<usize>().unwrap() {
                    start -= 1;
                    if start % 100 == 0 {
                        p2_res += 1;
                        start = 100
                    }
                }
                if start % 100 == 0 {
                    p1_res += 1;
                    start = 0
                }
            }
            "R" => {
                for _ in 1..=x.parse::<usize>().unwrap() {
                    start += 1;
                    if start % 100 == 0 {
                        p2_res += 1;
                        start = 0
                    }
                }
                if start % 100 == 0 {
                    p1_res += 1;
                    start = 0
                }
            }
            _ => unreachable!(),
        }
        //println!("{p1_res}, {p2_res}, {start}");
    }

    (p1_res, p2_res)
}

fn main() {
    let input = read_file_by_line("../inputs/day1.input");
    //let input = read_file_by_line("../inputs/day1_demo.input");
    //parse_input(&input);
    dbg!(day1(&input));
}
