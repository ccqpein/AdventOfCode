use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn calculate(mass: i32) -> i32 {
    mass / 3 - 2
}

fn calculate2(mass: i32) -> i32 {
    let mut sum_num = 0;
    let mut each_step = mass;
    loop {
        each_step = each_step / 3 - 2;
        if each_step <= 0 {
            break;
        }
        sum_num += each_step
    }
    sum_num
}

fn read_file_by_line(filepath: String) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    let mut buf_reader = BufReader::new(file).lines();

    let mut result = vec![];

    loop {
        if let Some(s) = buf_reader.next() {
            result.push(s.unwrap())
        } else {
            break;
        }
    }

    result
}

fn read_file_by_line2(filepath: String) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

fn main() {
    //dbg!(calculate(14));
    //dbg!(read_file_by_line(String::from("day1.input")));

    // println!(
    //     "{}",
    //     read_file_by_line(String::from("day1.input"))
    //         .iter()
    //         .map(|x| calculate(x.parse::<i32>().unwrap()))
    //         .sum::<i32>()
    // );

    //dbg!(calculate2(1969));

    println!(
        "{}",
        read_file_by_line2(String::from("day1.input"))
            .iter()
            .map(|x| calculate2(x.parse::<i32>().unwrap()))
            .sum::<i32>()
    );
}
