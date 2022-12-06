use tools::*;

fn day6(input: &Vec<String>) -> usize {
    let mut input: Vec<char> = input[0].chars().collect();
    let mut count = 0;

    loop {
        if !all_diff(&input[0..4], 4) {
            input.drain(..1);
            count += 1
        } else {
            return count + 4;
        }
    }
}

fn day6_part2(input: &Vec<String>) -> i64 {
    let mut input: Vec<char> = input[0].chars().collect();
    let mut count = 0;

    loop {
        if !all_diff(&input[0..14], 14) {
            input.drain(..1);
            count += 1
        } else {
            return count + 14;
        }
    }
}

fn all_diff(cs: &[char], len: usize) -> bool {
    for i in 0..len {
        for j in i + 1..len {
            if cs[i] == cs[j] {
                return false;
            }
        }
    }
    true
}

fn main() {
    //let input = read_file_by_line("./inputs/day6_demo.input")
    let input = read_file_by_line("./inputs/day6.input");
    println!("{}", day6(&input));
    println!("{}", day6_part2(&input));
}
