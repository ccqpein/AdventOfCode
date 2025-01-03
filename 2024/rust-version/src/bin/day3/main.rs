use regex::Regex;
use tools::*;

fn day3(input: &[String]) -> i32 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    let all = input
        .iter()
        .map(|line| {
            re.captures_iter(&line)
                .map(|c| c.extract())
                .map(|(_, [x, y])| vec![x, y])
            //.flatten();
            //.collect::<Vec<_>>()
        })
        .flatten()
        .collect::<Vec<_>>();

    all.iter()
        .map(|x| x[0].parse::<i32>().unwrap() * x[1].parse::<i32>().unwrap())
        .sum::<i32>()

    //dbg!(&all);
    //dbg!(all.len());
}

fn main() {
    let input = read_file_by_line("../inputs/day3.input");
    println!("Day 3: {}", day3(&input));
}
