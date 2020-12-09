use std::collections::HashSet;
use tools::{read_file_by_line, Ma, MaResult};

fn part1(ma: &mut Ma) -> MaResult {
    let mut set = HashSet::new();
    for status in ma {
        if set.contains(&status.status) {
            return status;
        }
        set.insert(status.status);
    }

    MaResult {
        status: 0,
        accumulator: 0,
    }
}

fn part2(ma: &mut Ma) -> i32 {
    let all_nop = ma
        .boot_code
        .iter()
        .enumerate()
        .filter(|(_, a)| a.0 == "nop")
        .map(|(ind, _)| ind)
        .collect::<Vec<usize>>();

    let all_jmp = ma
        .boot_code
        .iter()
        .enumerate()
        .filter(|(_, a)| a.0 == "jmp")
        .map(|(ind, _)| ind)
        .collect::<Vec<usize>>();

    for n in all_nop {
        ma.reset();
        ma.write_instruction_at(n, "jmp".to_string());
        if part1(ma).status == 0 {
            return ma.accumulator;
        }
        ma.write_instruction_at(n, "nop".to_string());
    }

    for n in all_jmp {
        ma.reset();
        ma.write_instruction_at(n, "nop".to_string());
        if part1(ma).status == 0 {
            return ma.accumulator;
        }
        ma.write_instruction_at(n, "jmp".to_string());
    }

    -1
}

fn main() {
    let input = read_file_by_line(String::from("./src/day8.input"));
    let input = input
        .iter()
        .map(|line| {
            let mut a = line.split_whitespace();
            (
                a.next().unwrap().to_string(),
                a.next().unwrap().parse::<i32>().unwrap(),
            )
        })
        .collect::<Vec<(String, i32)>>();

    let mut ma = Ma::new(input);
    println!("{:?}", part1(&mut ma));
    ma.reset();
    println!("{:?}", part2(&mut ma));
}
