use std::collections::VecDeque;

use regex::Regex;
use tools::*;

fn day3(inputs: &[String]) -> usize {
    let re = Regex::new(r"(\d+)\s+(\d+)\s+(\d+)").unwrap();
    let ii = inputs.into_iter().map(|line| {
        let (_, [a, b, c]) = re.captures(line).map(|x| x.extract()).unwrap();
        let a = a.parse::<i32>().unwrap();
        let b = b.parse::<i32>().unwrap();
        let c = c.parse::<i32>().unwrap();

        //println!("{},{},{}", a, b, c);
        let mut x = vec![a, b, c];
        x.sort();
        x
    });

    ii.filter_map(|x| if x[0] + x[1] > x[2] { Some(()) } else { None })
        .count()
}

fn day3_2(inputs: &[String]) -> usize {
    let re = Regex::new(r"(\d+)\s+(\d+)\s+(\d+)").unwrap();
    let (mut aa, mut bb, mut cc) = (VecDeque::new(), VecDeque::new(), VecDeque::new());
    inputs.into_iter().for_each(|line| {
        let (_, [a, b, c]) = re.captures(line).map(|x| x.extract()).unwrap();
        let a = a.parse::<i32>().unwrap();
        let b = b.parse::<i32>().unwrap();
        let c = c.parse::<i32>().unwrap();

        aa.push_back(a);
        bb.push_back(b);
        cc.push_back(c);
    });
    aa.append(&mut bb);
    aa.append(&mut cc);

    //dbg!(&aa);
    let mut count = 0;
    while aa.len() > 0 {
        let (a, b, c) = (
            aa.pop_front().unwrap(),
            aa.pop_front().unwrap(),
            aa.pop_front().unwrap(),
        );

        let mut x = vec![a, b, c];
        x.sort();
        if x[0] + x[1] > x[2] {
            count += 1;
        }
    }
    count
}

fn main() {
    let input = read_file_by_line("../inputs/day3.input");
    //let input = read_file_by_line("../inputs/day3_demo.input");
    println!("part1: {}", day3(&input));
    println!("part1: {}", day3_2(&input));
}
