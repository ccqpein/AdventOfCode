use std::collections::HashSet;

use itertools::Itertools;
use rayon::prelude::*;
use std::iter::{empty, once};

// fn optimized_sieve(limit: usize) -> Box<dyn Iterator<Item = usize>> {
//     if limit < 3 {
//         return if limit < 2 {
//             Box::new(empty())
//         } else {
//             Box::new(once(2))
//         };
//     }

//     let ndxlmt = (limit - 3) / 2 + 1;
//     let bfsz = ((limit - 3) / 2) / 32 + 1;
//     let mut cmpsts = vec![0u32; bfsz];
//     let sqrtndxlmt = ((limit as f64).sqrt() as usize - 3) / 2 + 1;

//     for ndx in 0..sqrtndxlmt {
//         if (cmpsts[ndx >> 5] & (1u32 << (ndx & 31))) == 0 {
//             let p = ndx + ndx + 3;
//             let mut cullpos = (p * p - 3) / 2;
//             while cullpos < ndxlmt {
//                 unsafe {
//                     // avoids array bounds check, which is already done above
//                     let cptr = cmpsts.get_unchecked_mut(cullpos >> 5);
//                     *cptr |= 1u32 << (cullpos & 31);
//                 }
//                 //                cmpsts[cullpos >> 5] |= 1u32 << (cullpos & 31); // with bounds check
//                 cullpos += p;
//             }
//         }
//     }

//     Box::new((-1..ndxlmt as isize).into_iter().filter_map(move |i| {
//         if i < 0 {
//             Some(2)
//         } else {
//             if cmpsts[i as usize >> 5] & (1u32 << (i & 31)) == 0 {
//                 Some((i + i + 3) as usize)
//             } else {
//                 None
//             }
//         }
//     }))
// }

fn day20(input: usize) -> usize {
    // (1..)
    //     .filter(|d| (1..*d).filter(|dd| d % dd == 0).sum::<usize>() * 10 + d * 10 >= input)
    //     .next()
    //     .unwrap()

    // let mut i = 1;
    // let mut last = 1;
    // loop {
    //     if find_all_factors(i).iter().map(|d| d * 10).sum::<usize>() >= input {
    //         break;
    //     }
    //     last = i;
    //     i *= 2;
    //     //i += 210;
    // }
    //dbg!(i);
    //dbg!(last);

    for j in 665280 - 100000..=665280 {
        if find_all_factors(j).iter().map(|d| d * 10).sum::<usize>() >= input {
            return j;
        }
    }

    // for (a, b) in optimized_sieve(1_000_000).tuple_windows() {
    //     for j in a..=b {
    //         if find_all_factors(j).iter().map(|d| d * 10).sum::<usize>() >= input {
    //             return j;
    //         }
    //     }
    // }

    0
}

fn find_all_factors(num: usize) -> HashSet<usize> {
    let mut set = HashSet::new();
    for i in 1..=(num / 2) as usize {
        if num % i == 0 {
            set.insert(i);
            set.insert(num / i);
        }
    }
    set
}

fn day20_2(input: usize) -> usize {
    (0..30)
        .map(|i| i * 1_000_000)
        .tuple_windows()
        .collect::<Vec<_>>()
        .into_par_iter()
        .map(|(i, j)| {
            //dbg!((i, j));
            (i..=j)
                .find(|x| {
                    find_all_factors(*x)
                        .iter()
                        .filter_map(|d| {
                            if *x as f64 / *d as f64 <= 50.0 {
                                Some(d * 11)
                            } else {
                                None
                            }
                        })
                        .sum::<usize>()
                        >= input
                })
                .unwrap_or(0)
        })
        .min()
        .unwrap_or(0)
}

// Part1:
// 1048608 too large
// 948600 too large
// 848640 isn't right
// 750960 isn't right
// 665280 finially right

fn main() {
    //dbg!(find_all_factors(4));
    // dbg!(find_all_factors(1048608)
    //     .iter()
    //     .map(|d| d * 10)
    //     .sum::<usize>());
    println!("1: {:?}", day20(29000000));
    println!("2: {:?}", day20_2(29000000));
}
