use std::collections::HashSet;

use tools::*;

fn part1(input: &Vec<String>) -> usize {
    let mut result = 0;
    for i in 0..input.len() {
        for j in 0..input[0].len() {
            let this = input[i].get(j..j + 1).unwrap();
            let (x, y) = (i as i32, j as i32);
            if [
                //(x - 1, y - 1),
                (x, y - 1),
                //(x + 1, y - y),
                (x - 1, y),
                (x + 1, y),
                //(x - 1, y + 1),
                (x, y + 1),
                //(x + 1, y + 1),
            ]
            .iter()
            .filter_map(|(x, y)| {
                if let Some(l) = input.get(*x as usize) {
                    l.get((*y as usize)..((y + 1) as usize))
                } else {
                    None
                }
            })
            .all(|v| v > this)
            {
                result += this.parse::<usize>().unwrap() + 1
            }
        }
    }
    result
}

fn part2(input: &Vec<String>) -> usize {
    //let all_low_point = vec![];
    let mut result = 0;
    let mut record = vec![];
    for i in 0..input.len() {
        for j in 0..input[0].len() {
            let this = input[i].get(j..j + 1).unwrap();
            let (x, y) = (i as i32, j as i32);
            if [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
                .iter()
                .filter_map(|(x, y)| {
                    if let Some(l) = input.get(*x as usize) {
                        l.get((*y as usize)..((y + 1) as usize))
                    } else {
                        None
                    }
                })
                .all(|v| v > this)
            {
                let mut this_record = HashSet::new();
                this_record.insert((i as i32, j as i32));
                find(
                    this.parse().unwrap(),
                    (i as i32, j as i32),
                    &mut this_record,
                    input,
                );
                record.push(this_record.len());
            }
        }
    }
    println!("{:?}", record);
    record.sort();
    record.iter().rev().take(3).product()
}

fn find(this: usize, (x, y): (i32, i32), record: &mut HashSet<(i32, i32)>, input: &Vec<String>) {
    [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
        .iter()
        .filter_map(|(x, y)| {
            if let Some(l) = input.get(*x as usize) {
                if let Some(c) = (l.get((*y as usize)..((y + 1) as usize))) {
                    Some((c, (x, y)))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .for_each(|(p, (x, y))| {
            if p.parse::<usize>().unwrap() > this
                && p.parse::<usize>().unwrap() != 9
                && !record.contains(&(*x, *y))
            {
                record.insert((*x, *y));
                find(p.parse::<usize>().unwrap(), (*x, *y), record, input)
            }
        });
}

fn main() {
    let input = read_file_by_line("./src/bin/day9/day9.input");
    //let input = read_file_by_line("./src/bin/day9/day9_demo.input");
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
