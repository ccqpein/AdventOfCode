use std::str::Chars;
use tools::read_file_by_line;

fn convert_2_chars(sv: &Vec<String>) -> Vec<Chars> {
    sv.iter().map(|s| s.chars()).collect()
}

fn part1(mut vc: Vec<Chars>, line_count: usize) -> usize {
    let (mut count, mut col) = (0, 0);
    for row in 1..vc.len() {
        col += 3;
        if col >= line_count {
            col -= line_count;
        }
        if vc[row].nth(col).unwrap() == '#' {
            count += 1
        }
    }
    count
}

fn part2_helper(mut vc: Vec<Chars>, rule: (usize, usize), line_count: usize) -> usize {
    let (mut count, mut col, mut row) = (0, 0, 0);
    while row < vc.len() - 1 {
        col += rule.0;
        row += rule.1;
        if col >= line_count {
            col -= line_count;
        }
        if vc[row].nth(col).unwrap() == '#' {
            count += 1
        }
    }
    count
}

fn part2(vc: Vec<Chars>, rules: &[(usize, usize)], line_count: usize) -> usize {
    rules
        .iter()
        .map(|rule| part2_helper(vc.clone(), *rule, line_count))
        .fold(1, |acc, x| acc * x)
}

fn main() {
    let input = read_file_by_line(String::from("./src/day3.input"));
    let line_num = input[0].len();
    let input = convert_2_chars(&input);

    println!("part1 answer: {:?}", part1(input.clone(), line_num));
    println!(
        "part2 answer: {:?}",
        part2(
            input,
            &vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)],
            line_num
        )
    );
}
