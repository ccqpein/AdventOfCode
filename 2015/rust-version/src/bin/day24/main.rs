#![feature(let_chains)]
use std::collections::HashSet;

use tools::*;

// fn get_group(
//     bucket: &HashSet<i64>,
//     mut already: HashSet<i64>,
//     need: i64,
// ) -> Option<Vec<HashSet<i64>>> {
//     if let Some(_) = bucket.get(&need)
//         && !already.contains(&need)
//     {
//         already.insert(need);
//         return Some(vec![already]);
//     }

//     let mut bb = bucket
//         .iter()
//         .filter(|d| **d < need && !already.contains(d))
//         .filter_map(|d| {
//             let mut aa = already.clone();
//             aa.insert(*d);
//             get_group(bucket, aa, need - d)
//         })
//         .flatten()
//         .collect::<Vec<HashSet<_>>>();

//     'xx: loop {
//         for i in 0..bb.len() {
//             for j in i + 1..bb.len() {
//                 if bb[j] == bb[i] {
//                     bb.remove(j);
//                     continue 'xx;
//                     //temp.push(j);
//                 }
//             }
//         }
//         break;
//     }

//     if bb.len() == 0 {
//         None
//     } else {
//         Some(bb)
//     }
// }

// fn day24(s: &HashSet<i64>) -> i64 {
//     let av_v = (s.iter().sum::<i64>() as f64 / 3.0) as i64;
//     let mut gs = get_group(s, HashSet::new(), av_v).unwrap();

//     gs.sort_by(|a, b| a.len().partial_cmp(&b.len()).unwrap());
//     let s_len = gs[0].len();

//     gs.iter()
//         .filter(|s| s.len() == s_len)
//         .map(|s| s.iter().product())
//         .min()
//         .unwrap()
// }

fn get_group(start: usize, left: usize, need: i64, input: &[i64]) -> Option<Vec<HashSet<i64>>> {
    if left == 0 {
        return None;
    }

    let mut result = vec![];
    for i in start..input.len() {
        if input[i] == need && left == 1 {
            let mut ss = HashSet::new();
            ss.insert(input[i]);
            result.push(ss);
        } else if input[i] < need {
            if let Some(x) = get_group(i + 1, left - 1, need - input[i], input) {
                for mut xx in x {
                    xx.insert(input[i]);
                    result.push(xx);
                }
            }
        }
    }

    if result.len() == 0 {
        None
    } else {
        Some(result)
    }
}

fn day24(input: &[i64], p2: bool) -> i64 {
    let av_v = if p2 {
        (input.iter().sum::<i64>() as f64 / 4.0) as i64
    } else {
        (input.iter().sum::<i64>() as f64 / 3.0) as i64
    };
    for left in 2..=input.len() {
        if let Some(gg) = get_group(0, left, av_v, input) {
            return gg.iter().map(|g| g.iter().product()).min().unwrap();
        }
    }
    0
}

fn main() {
    let input = vec![1, 2, 3, 4, 5, 7, 8, 9, 10, 11]
        .into_iter()
        .collect::<Vec<i64>>();

    let mut input = read_file_by_line("../inputs/day24.input")
        .into_iter()
        .map(|d| d.parse::<i64>().unwrap())
        .collect::<Vec<i64>>();
    input.sort_by(|a, b| b.partial_cmp(a).unwrap()); // larger first

    //dbg!(get_group(0, 3, 20, &input));
    println!("1: {}", day24(&input, false)); // 11266889531
    println!("2: {}", day24(&input, true)); // 77387711
}
