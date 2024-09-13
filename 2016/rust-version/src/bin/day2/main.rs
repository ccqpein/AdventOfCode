use std::cell::LazyCell;

use tools::*;

fn next_number(number: &mut u8, dir: &char) {
    match dir {
        &'U' => {
            if *number > 3 {
                *number -= 3
            }
        }
        &'D' => {
            if *number < 7 {
                *number += 3
            }
        }
        &'R' => {
            if *number != 3 && *number != 6 && *number != 9 {
                *number += 1
            }
        }
        &'L' => {
            if *number != 1 && *number != 4 && *number != 7 {
                *number -= 1
            }
        }
        _ => unreachable!(),
    }
}

fn day2(input: &[String]) -> Vec<u8> {
    let mut res = vec![];
    let mut number = 5;
    for l in input.iter() {
        for c in l.chars() {
            next_number(&mut number, &c)
        }
        res.push(number)
    }

    res
}

static KB: [[Option<char>; 5]; 5] = [
    [None, None, Some('1'), None, None],
    [None, Some('2'), Some('3'), Some('4'), None],
    [Some('5'), Some('6'), Some('7'), Some('8'), Some('9')],
    [None, Some('A'), Some('B'), Some('C'), None],
    [None, None, Some('D'), None, None],
];

fn next_step(coop: &mut (usize, usize), dir: &char) {
    match dir {
        &'U' => {
            if coop.0 == 0 {
                return;
            }

            if KB[coop.0 - 1][coop.1].is_none() {
                return;
            }

            coop.0 -= 1
        }
        &'D' => {
            if coop.0 == 4 {
                return;
            }

            if KB[coop.0 + 1][coop.1].is_none() {
                return;
            }

            coop.0 += 1
        }
        &'R' => {
            if coop.1 == 4 {
                return;
            }

            if KB[coop.0][coop.1 + 1].is_none() {
                return;
            }

            coop.1 += 1
        }
        &'L' => {
            if coop.1 == 0 {
                return;
            }

            if KB[coop.0][coop.1 - 1].is_none() {
                return;
            }

            coop.1 -= 1
        }
        _ => unreachable!(),
    }
}

fn day2_2(input: &[String]) -> Vec<char> {
    let mut res = vec![];
    let mut number = (2_usize, 0_usize);
    for l in input.iter() {
        for c in l.chars() {
            next_step(&mut number, &c)
        }
        res.push(number)
    }

    res.into_iter().map(|(r, c)| KB[r][c].unwrap()).collect()
}

fn main() {
    let input = read_file_by_line("../inputs/day2.input");
    //let input = read_file_by_line("../inputs/day2_demo.input");
    //dbg!(input);
    println!("part1: {:?}", day2(&input));
    println!("part1: {:?}", day2_2(&input));
}
