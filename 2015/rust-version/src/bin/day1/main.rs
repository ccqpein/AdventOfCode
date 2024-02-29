use tools::*;

fn day1(input: &[String]) -> i32 {
    let input = input[0].chars();

    input
        .map(|c| match c {
            '(' => 1,
            ')' => -1,
            _ => unreachable!(),
        })
        .sum()
}

fn day1_part2(input: &[String]) -> i32 {
    let input = input[0].chars();
    let mut sum = 0;

    for (ind, c) in input.enumerate() {
        match c {
            '(' => sum += 1,
            ')' => sum -= 1,
            _ => unreachable!(),
        }
        if sum < 0 {
            return ind as i32 + 1;
        }
    }
    0
}

fn main() {
    //let input = vec!["()())".to_string()];
    let input = read_file_by_line("../inputs/day1.input");

    println!("{:?}", day1(&input));
    println!("{:?}", day1_part2(&input));
}
