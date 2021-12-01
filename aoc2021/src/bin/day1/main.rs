use tools::*;

fn part1(input: Vec<String>) {
    let mut count = 0;
    let mut last = input[0].parse::<i32>().unwrap();
    for line in &input[1..] {
        let this = line.parse::<i32>().unwrap();
        if this > last {
            count += 1
        }
        last = this;
    }

    println!("{}", count);
}

fn part2(input: Vec<String>) {
    let mut result = 0;
    let mut input = input
        .iter()
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();
    let mut last = input.get(0..3).unwrap().iter().sum();
    for i in 1..input.len() {
        match (input.get(i), input.get(i + 1), input.get(i + 2)) {
            (Some(a), Some(b), Some(c)) => {
                let this = a + b + c;
                if this > last {
                    result += 1;
                }
                last = this;
            }
            _ => break,
        }
    }

    println!("{}", result);
}

fn main() {
    let input = read_file_by_line("./src/bin/day1/day1.input");
    part1(input);
    part2(input);
}
