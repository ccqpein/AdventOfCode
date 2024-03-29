#![feature(let_chains)]
use std::collections::HashSet;

use tools::*;

fn get_group(
    bucket: &HashSet<i64>,
    mut already: HashSet<i64>,
    need: i64,
) -> Option<Vec<HashSet<i64>>> {
    if let Some(_) = bucket.get(&need)
        && !already.contains(&need)
    {
        already.insert(need);
        return Some(vec![already]);
    }

    let mut bb = bucket
        .iter()
        .filter(|d| **d < need && !already.contains(d))
        .filter_map(|d| {
            let mut aa = already.clone();
            aa.insert(*d);
            get_group(bucket, aa, need - d)
        })
        .flatten()
        .collect::<Vec<HashSet<_>>>();

    'xx: loop {
        for i in 0..bb.len() {
            for j in i + 1..bb.len() {
                if bb[j] == bb[i] {
                    bb.remove(j);
                    continue 'xx;
                    //temp.push(j);
                }
            }
        }
        break;
    }

    if bb.len() == 0 {
        None
    } else {
        Some(bb)
    }
}

fn day24(s: &HashSet<i64>) -> i64 {
    let av_v = (s.iter().sum::<i64>() as f64 / 3.0) as i64;
    let mut gs = get_group(s, HashSet::new(), av_v).unwrap();

    gs.sort_by(|a, b| a.len().partial_cmp(&b.len()).unwrap());
    let s_len = gs[0].len();

    gs.iter()
        .filter(|s| s.len() == s_len)
        .map(|s| s.iter().product())
        .min()
        .unwrap()
}

fn main() {
    let input = read_file_by_line("../inputs/day24.input")
        .into_iter()
        .map(|d| d.parse::<i64>().unwrap())
        .collect::<HashSet<i64>>();

    // let input = vec![1, 2, 3, 4, 5, 7, 8, 9, 10, 11]
    //     .into_iter()
    //     .collect::<HashSet<i64>>();

    //dbg!(get_group(&input, HashSet::new(), 20));
    println!("1: {}", day24(&input));
}
