#![feature(slice_pattern)]

use tools::*;

fn part1(input: &Vec<String>) {
    let mut forward = 0;
    let mut depth = 0;

    for line in input {
        match line.split_whitespace().collect::<Vec<&str>>().as_slice() {
            ["forward", f] => {
                forward += f.parse::<i32>().unwrap();
            }
            ["down", d] => {
                depth += d.parse::<i32>().unwrap();
            }
            ["up", u] => {
                depth -= u.parse::<i32>().unwrap();
            }
            _ => {
                panic!()
            }
        }
    }

    println!("{}", forward * depth)
}

fn part2(input: &Vec<String>) {
    let mut forward = 0;
    let mut depth = 0;
    let mut aim = 0;

    for line in input {
        match line.split_whitespace().collect::<Vec<&str>>().as_slice() {
            ["forward", f] => {
                forward += f.parse::<i32>().unwrap();
                depth += f.parse::<i32>().unwrap() * aim;
            }
            ["down", d] => {
                aim += d.parse::<i32>().unwrap();
            }
            ["up", u] => {
                aim -= u.parse::<i32>().unwrap();
            }
            _ => {
                panic!()
            }
        }
    }

    println!("{}", forward * depth)
}

fn main() {
    let input = read_file_by_line("./src/bin/day2/day2.input");
    part1(&input);
    part2(&input);
}
