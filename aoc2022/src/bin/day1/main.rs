use itertools::Itertools;
use tools::*;

fn day1(input: &Vec<String>) -> i64 {
    let mut elves = vec![];
    let mut cache = 0;
    for line in input {
        if line == "" {
            elves.push(cache);
            cache = 0;
        } else {
            cache += line.parse::<i64>().unwrap();
        }
    }
    elves.into_iter().max().unwrap()
}

fn day1_part2(input: &Vec<String>) -> i64 {
    let mut elves = vec![];
    let mut cache = 0;
    for line in input {
        if line == "" {
            elves.push(cache);
            cache = 0;
        } else {
            cache += line.parse::<i64>().unwrap();
        }
    }
    elves.sort_by(|a, b| b.partial_cmp(a).unwrap());
    elves.get(0..3).unwrap().iter().sum()
}

fn main() {
    //let input = read_file_by_line("./inputs/day1_demo.input");
    let input = read_file_by_line("./inputs/day1.input");
    println!("{}", day1(&input));

    let input = read_file_by_line("./inputs/day1.input");
    println!("{}", day1_part2(&input));
}
