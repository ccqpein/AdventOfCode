use tools::*;

fn day4(input: &Vec<String>) -> i64 {
    let mut result = 0;
    for line in input {
        let mut ss = line
            .split(',')
            .map(|w| w.split('-'))
            .flatten()
            .map(|s| s.parse::<i32>().unwrap());

        let (a0, a1, b0, b1) = (
            ss.next().unwrap(),
            ss.next().unwrap(),
            ss.next().unwrap(),
            ss.next().unwrap(),
        );

        if ((a0 <= b0) && (a1 >= b1)) || ((b0 <= a0) && (b1 >= a1)) {
            result += 1;
        }
    }
    result
}

fn day4_part2(input: &Vec<String>) -> i64 {
    let mut result = 0;
    for line in input {
        let mut ss = line
            .split(',')
            .map(|w| w.split('-'))
            .flatten()
            .map(|s| s.parse::<i32>().unwrap());

        let (a0, a1, b0, b1) = (
            ss.next().unwrap(),
            ss.next().unwrap(),
            ss.next().unwrap(),
            ss.next().unwrap(),
        );

        if ((a0 < b0) && (a1 < b0)) || ((b0 < a0) && (b1 < a0)) {
        } else {
            result += 1;
        }
    }
    result
}

fn main() {
    //let input = read_file_by_line("./inputs/day4_demo.input")
    let input = read_file_by_line("./inputs/day4.input");
    println!("{}", day4(&input));
    println!("{}", day4_part2(&input));
}
