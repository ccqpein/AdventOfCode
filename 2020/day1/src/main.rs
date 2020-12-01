use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn read_file_by_line(filepath: String) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

fn find_2020(input: &[String]) -> (u32, u32) {
    let input: Vec<u32> = input.iter().map(|s| s.parse::<u32>().unwrap()).collect();
    for i in 0..input.len() {
        for j in i..input.len() {
            if input[i] + input[j] == 2020 {
                return (input[i], input[j]);
            }
        }
    }
    (0, 0)
}

fn find_2020_part2(input: &[String]) -> (u32, u32, u32) {
    let input: Vec<u32> = input.iter().map(|s| s.parse::<u32>().unwrap()).collect();
    for i in 0..input.len() {
        for j in i..input.len() {
            for z in j..input.len() {
                if input[i] + input[j] + input[z] == 2020 {
                    return (input[i], input[j], input[z]);
                }
            }
        }
    }
    (0, 0, 0)
}

fn main() {
    let input = read_file_by_line(String::from("./src/day1.input"));
    let (i, j) = find_2020(&input);
    println!("result 1: {}", i * j);

    let (i, j, z) = find_2020_part2(&input);
    println!("result 2: {}", i * j * z);
}
