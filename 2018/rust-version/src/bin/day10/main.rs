use std::collections::HashSet;

use rayon::prelude::*;
use regex::*;
use tools::*;

fn day10(input: &[String]) {
    let re =
        Regex::new(r#"position=< ?([-]?\d+),  ?([-]?\d+)> velocity=< ?([-]?\d+),  ?([-]?\d+)>"#)
            .unwrap();

    let mut bucket = input
        .iter()
        .map(|l| {
            let (_, [x, y, dx, dy]) = re.captures(l).unwrap().extract();
            //println!("{x} {y} {dx} {dy}");
            (
                x.parse().unwrap(),
                y.parse().unwrap(),
                dx.parse().unwrap(),
                dy.parse().unwrap(),
            )
        })
        .collect::<Vec<(i32, i32, i32, i32)>>();

    let mut t = 1;
    loop {
        //println!("{}:", t);
        let ss = bucket
            .iter()
            .map(|(x, y, _, _)| (*x, *y))
            .collect::<HashSet<_>>();

        if all_around(&ss) {
            println!("{}:", t);
            view(&ss);
            break;
        }
        bucket = update(bucket);
        t += 1;
    }
}

fn update(b: Vec<(i32, i32, i32, i32)>) -> Vec<(i32, i32, i32, i32)> {
    b.into_par_iter()
        .map(|(x, y, dx, dy)| (x + dx, y + dy, dx, dy))
        .collect()
}

fn view(bucket: &HashSet<(i32, i32)>) {
    let min_x = bucket.iter().map(|(x, _)| *x).min().unwrap();
    let max_x = bucket.iter().map(|(x, _)| *x).max().unwrap();

    let min_y = bucket.iter().map(|(_, y)| *y).min().unwrap();
    let max_y = bucket.iter().map(|(_, y)| *y).max().unwrap();

    for y in min_y..=max_y {
        println!("");
        for x in min_x..=max_x {
            if bucket.contains(&(x, y)) {
                print!("#")
            } else {
                print!(".")
            }
        }
    }
}

fn all_around(bucket: &HashSet<(i32, i32)>) -> bool {
    for (x, y) in bucket {
        if [(1, 0), (0, 1), (-1, 0), (0, -1)]
            .into_iter()
            .all(|(dx, dy)| !bucket.contains(&(x + dx, y + dy)))
        {
            return false;
        }
    }
    true
}

fn main() {
    let input = read_file_by_line("../inputs/day10.input");
    //let input = read_file_by_line("../inputs/day10_demo.input");

    day10(&input);
}
