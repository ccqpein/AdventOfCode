use tools::*;

fn part1(input: &Vec<String>) -> i32 {
    let mut content = input[0]
        .split(",")
        .map(|c| c.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    content.sort();
    let mut result = i32::MAX;
    //let mut step = ;
    for d in 0..*content.last().unwrap() as usize {
        let temp = content.iter().map(|e| (*e - d as i32).abs()).sum::<i32>();
        if temp < result {
            result = temp;
        }
    }

    result
}

fn step_consume(step: i32) -> i32 {
    let mut a = 1;
    let mut result = 0;
    for _ in 0..(step as usize) {
        result += a;
        a += 1;
    }
    result
}

fn part2(input: &Vec<String>) -> i32 {
    let mut content = input[0]
        .split(",")
        .map(|c| c.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    content.sort();
    let mut result = i32::MAX;
    //let mut step = ;
    for d in 0..*content.last().unwrap() as usize {
        let temp = content
            .iter()
            .map(|e| step_consume((*e - d as i32).abs()))
            .sum::<i32>();
        if temp < result {
            result = temp;
        }
    }

    result
}

fn main() {
    let input = read_file_by_line("./src/bin/day7/day7.input");
    //let input = read_file_by_line("./src/bin/day7/day7_demo.input");
    //println!("{}", part1(&input));
    println!("{}", part2(&input));
}
