use std::collections::{HashMap, HashSet};

use tools::*;

fn part1(input: &Vec<String>) -> i32 {
    let input = input
        .iter()
        .map(|l| {
            l.chars()
                .map(|c| c.to_string().parse::<i32>().unwrap())
                .collect()
        })
        .collect::<Vec<Vec<i32>>>();

    let map = Map::from(input.clone());

    let end = (input.len() - 1, input[0].len() - 1);

    helper_v3(&map, (0, 0), end)
}

// fn helper(
//     m: &Map<i32>,
//     record: HashSet<(usize, usize)>,
//     (x, y): (usize, usize),
//     end: (usize, usize),
// ) -> Option<i32> {
//     if x == end.0 && y == end.1 {
//         return Some(m.get(end).unwrap());
//     }

//     let mut result = vec![];
//     for ((i, j), v) in m.get_around((x, y)) {
//         if record.contains(&(i, j)) {
//             continue;
//         }
//         let mut new_record = record.clone();
//         new_record.insert((i, j));
//         if let Some(next_v) = helper(m, new_record, (i, j), end) {
//             result.push(v + next_v);
//         }
//     }

//     result.iter().min().cloned()
// }

/*
fn helper_v2(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) {
    let mut record: HashMap<((usize, usize), (usize, usize)), i32> = HashMap::new();
    let mut this = start;
    //let mut stack = m.get_around(this);
    //stack.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap());
    m.get_around(this).into_iter().for_each(|(coorp, v)| {
        record.insert((start, coorp), v);
    });
    let mut already = HashSet::new();
    let mut next_start: Vec<((usize, usize), i32)> = vec![];
    loop {
        // let mut all_so_far: Vec<i32> = record.values().cloned().collect();
        // all_so_far.sort();
        // let next_start = record
        //     .iter()
        //     .filter(|(_, v)| **v == all_so_far[0])
        //     .map(|(coorp, v)| (coorp.clone(), v.clone()))
        //     .collect::<Vec<_>>();

        let mut new_next_start = vec![];
        next_start.iter().for_each(|(tail, v)| {
            let next_round = m.get_around(*tail);
            new_next_start.append(&mut next_round.clone());
            next_round
                .into_iter()
                .for_each(|(coorp, new_v)| match record.get(&(start, coorp)) {
                    Some(old_v) => {
                        if *old_v >= v + new_v {
                            record.insert((start, coorp), v + new_v);
                        }
                    }
                    None => {
                        record.insert((start, coorp), v + new_v);
                    }
                })
        });

        next_start = new_next_start;
    }
}*/

fn helper_v3(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) -> i32 {
    let mut record = HashMap::new();
    record.insert(start, 0);
    let mut already = HashSet::new();
    let mut this = start;
    let mut cache = vec![];
    loop {
        if let Some(v) = record.get(&end) {
            return *v;
        }
        let this_v = record.get(&this).cloned().unwrap();
        let next_round = m.get_around_horiz(this);

        for p in next_round.clone() {
            if !already.contains(&p.0) {
                match record.get(&p.0) {
                    Some(old_v) => {
                        if *old_v >= this_v + p.1 {
                            record.insert(p.0, this_v + p.1);
                            cache.push((p.0, this_v + p.1))
                        } else {
                            cache.push((p.0, *old_v))
                        }
                    }
                    None => {
                        record.insert(p.0, this_v + p.1);
                        cache.push((p.0, this_v + p.1))
                    }
                }
            }
        }

        already.insert(this);

        // sort cache
        cache.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap());
        cache = cache
            .iter()
            .filter(|(coorp, _)| !already.contains(coorp))
            .cloned()
            .collect();

        this = if let Some(aa) = cache.get(0) {
            aa.0
        } else {
            break;
        };
    }
    *record.get(&end).unwrap()
}

fn part2(input: &Vec<String>) -> i32 {
    let mut cache = vec![];
    for l in input {
        cache.push(dup_line(
            l.chars()
                .map(|c| c.to_string().parse::<i32>().unwrap())
                .collect(),
        ))
    }

    cache = dup_block(cache);

    let map = Map::from(cache.clone());

    let end = (cache.len() - 1, cache[0].len() - 1);

    dbg!(end);
    helper_v3(&map, (0, 0), end)
}

fn dup_line(mut l: Vec<i32>) -> Vec<i32> {
    let mut a: Vec<i32> = l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    let mut b: Vec<i32> = a.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    let mut c: Vec<i32> = b.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    let mut d: Vec<i32> = c.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    l.append(&mut a);
    l.append(&mut b);
    l.append(&mut c);
    l.append(&mut d);
    l
}

fn dup_block(mut ll: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    let mut a: Vec<Vec<i32>> = ll
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();
    let mut b: Vec<Vec<i32>> = a
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();
    let mut c: Vec<Vec<i32>> = b
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();
    let mut d: Vec<Vec<i32>> = c
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();

    ll.append(&mut a);
    ll.append(&mut b);
    ll.append(&mut c);
    ll.append(&mut d);
    ll
}

fn main() {
    let input = read_file_by_line("./src/bin/day15/day15.input");
    //let input = read_file_by_line("./src/bin/day15/day15_demo.input");
    //println!("{:?}", part1(&input));
    println!("{:?}", part2(&input));
}
