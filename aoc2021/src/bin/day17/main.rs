use std::{
    collections::HashSet,
    ops::{Range, RangeInclusive},
};

use tools::*;

fn part1(x: RangeInclusive<i32>, y: RangeInclusive<i32>) -> (i32, usize) {
    let mut bucket_y = vec![];
    let mut all_init = HashSet::new();
    for xx in 1..=*x.end() {
        for yy in -1000..1000 {
            let mut step = 0;
            let mut point = (0, 0);
            let mut flag = false;
            let mut highest_y = 0;
            loop {
                point = next_point(point, xx, yy, step);
                highest_y = i32::max(highest_y, point.1);
                if x.contains(&point.0) && y.contains(&point.1) {
                    flag = true;
                    all_init.insert((xx, yy));
                }

                if point.0 > *x.end() || point.1 < *y.start() {
                    break;
                }
                step += 1;
            }

            if flag {
                bucket_y.push(highest_y)
            }
        }
    }

    (*bucket_y.iter().max().unwrap(), all_init.len())
}

fn next_point((x, y): (i32, i32), xx: i32, yy: i32, step: i32) -> (i32, i32) {
    let re_x = if xx <= step { x } else { x + xx - step };
    let re_y = y + (yy - step);

    (re_x, re_y)
}

fn main() {
    //let input = read_file_by_line("./src/bin/day17/day17.input");
    //let input = read_file_by_line("./src/bin/day17/day17_demo.input");
    println!(
        "highest and init number is {:?}",
        part1((20..=30), (-10..=-5))
    );
    println!(
        "highest and init number is {:?}",
        part1((60..=94), (-171..=-136))
    );
}
