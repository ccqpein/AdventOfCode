use itertools::*;
use tools::*;

fn parse_input(input: &str) -> Vec<&str> {
    input.split(",").collect()
}

fn day11(input: &[&str]) -> i64 {
    let mut hor = 0f64;
    let mut ver = 0f64;

    let mut max_step = 0f64;

    input.into_iter().for_each(|s| {
        match *s {
            "n" => hor += 1.0,
            "nw" => {
                hor += 0.5;
                ver -= 1.0;
            }
            "ne" => {
                hor += 0.5;
                ver += 1.0;
            }
            "s" => {
                hor -= 1.0;
            }
            "sw" => {
                hor -= 0.5;
                ver -= 1.0;
            }
            "se" => {
                hor -= 0.5;
                ver += 1.0;
            }
            _ => unreachable!(),
        };

        let mm = cal_step(&hor, &ver);
        if mm > max_step {
            max_step = mm
        }
    });

    dbg!(hor);
    dbg!(ver);
    dbg!(max_step);
    0
}

fn cal_step(hor: &f64, ver: &f64) -> f64 {
    let hor = hor.abs();
    let ver = ver.abs();

    ver + (hor - (ver * 0.5))
}

fn main() {
    let input = read_file_by_line("../inputs/day11.input");

    day11(&parse_input("ne,ne,ne"));
    day11(&parse_input(&input[0]));
    // hor = 445.5
    // ver = 473.0
    // 473 + (445.5 - (473 * 0.5)) => part1 answer
}
