use itertools::*;
use tools::*;

fn day1(inputs: &[String]) -> i32 {
    let input = inputs[0]
        .chars()
        .map(|c| c.to_string())
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    //input.push(input[0]);
    //dbg!(input);
    input
        .iter()
        .circular_tuple_windows()
        .into_iter()
        .map(|(a, b)| if a == b { *a } else { 0 })
        .sum()
}

fn day1_2(inputs: &[String]) -> i32 {
    let input = inputs[0]
        .chars()
        .map(|c| c.to_string())
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    //input.push(input[0]);
    //dbg!(input);
    let len = input.len() as i32;
    let aa = input.into_iter().chunks((len / 2) as usize);

    let mut aa = aa.into_iter();
    let x = aa.next().unwrap();
    let y = aa.next().unwrap();
    std::iter::zip(x, y)
        .map(|(a, b)| if a == b { a + b } else { 0 })
        .sum()
}

fn main() {
    let input = read_file_by_line("../inputs/day1.input");
    dbg!(day1(&input));
    //dbg!(day1_2(&vec!["1212".to_string()]));
    //dbg!(day1_2(&vec!["12131415".to_string()]));
    //dbg!(day1_2(&vec!["123123".to_string()]));
    dbg!(day1_2(&input));
}
