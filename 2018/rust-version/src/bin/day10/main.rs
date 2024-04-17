use std::collections::HashSet;

use regex::*;
use tools::*;

fn day10(input: &[String]) {
    let re = Regex::new(r#"position=<([- ]?\d+), ([- ]?\d+)> velocity=<([- ]?\d+), ([- ]?\d+)>"#)
        .unwrap();

    let mut bucket = input
        .iter()
        .map(|l| {
            let (_, [x, y, dx, dy]) = re.captures(l).unwrap().extract();
            (
                x.parse().unwrap(),
                y.parse().unwrap(),
                dx.parse().unwrap(),
                dy.parse().unwrap(),
            )
        })
        .collect::<Vec<(i32, i32, i32, i32)>>();
}

fn view(bucket: &[(i32, i32, i32, i32)]) {}

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
}
