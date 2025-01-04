#![feature(str_as_str)]

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

fn day3_2(input: &[String]) -> i32 {
    let re = Regex::new(r"(mul\((\d+),(\d+)\)|do\(\)|don\'t\(\))").unwrap();
    let all = input
        .iter()
        .map(
            |line| re.captures_iter(&line), //.collect::<Vec<_>>()
        )
        .flatten()
        .collect::<Vec<_>>();
    //dbg!(&all);
    //dbg!(all.len());

    let mut flag = true;
    let mut result = 0;

    for a in all {
        //dbg!(&a);

        match a[0].as_str() {
            "do()" => {
                //dbg!("do");
                flag = true;
            }
            "don't()" => {
                //dbg!("don't");
                flag = false;
            }
            _ => {
                let x = a[2].parse::<i32>().unwrap();
                let y = a[3].parse::<i32>().unwrap();
                result += if flag { x * y } else { 0 };
            }
        }
    }
    result
}

fn main() {
    let input = read_file_by_line("../inputs/day3.input");
    println!("Day 3: {}", day3(&input));
    println!("Day 3 part 2: {}", day3_2(&input));
}
