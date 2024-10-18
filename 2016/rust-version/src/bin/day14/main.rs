use std::collections::{HashMap, HashSet};

use itertools::Itertools;

fn cal_md5(pre: &str, number: usize) -> String {
    format!("{:x}", md5::compute(format!("{pre}{}", number).as_bytes()))
}

fn cal_md5_2(pre: String) -> String {
    format!("{:x}", md5::compute(format!("{pre}").as_bytes()))
}

fn has_n_pad(s: &str, n: usize) -> Option<char> {
    let chars = s.chars().collect::<Vec<_>>();
    for i in 0..chars.len().saturating_sub(n - 1) {
        if chars.get(i..i + n).unwrap().iter().all_equal() {
            //dbg!(chars.get(i..i + n));
            //dbg!(chars.get(i..i + n).iter().all_equal_value());
            //dbg!(chars[i] == chars[i + 1]);
            return Some(chars[i]);
        }
    }

    None
}

// fn part1(pre: &str, mut count: usize) -> usize {
//     let mut five_table: HashMap<_, Vec<_>> = HashMap::new();
//     let mut have_fived = HashSet::new();

//     for i in 0.. {
//         let hh = cal_md5(pre, i);
//         let a = has_n_pad(&hh, 3);

//         if let Some(aa) = a {
//             // only first
//             match five_table.get(&aa) {
//                 Some(bucket) => {
//                     if let Some(jj) = bucket.iter().find(|&&v| v <= i + 1000 && v > i) {
//                         // dbg!(i);
//                         // dbg!(jj);
//                         // dbg!(&aa);

//                         count -= 1;
//                         if count == 0 {
//                             //dbg!(&aa);
//                             //dbg!(five_table.get(&aa));
//                             return i;
//                         }
//                     }
//                 }
//                 None => (),
//             }
//         }

//         for j in i + 1..=i + 2000 {
//             if have_fived.contains(&j) {
//                 continue;
//             }
//             let hh = cal_md5(pre, j);
//             if let Some(aa) = has_n_pad(&hh, 5) {
//                 // if a.len() > 1 {
//                 //     dbg!(&a);
//                 // }
//                 //dbg!(&hh);
//                 //dbg!(&aa);

//                 five_table.entry(aa).or_insert(vec![]).push(j);
//             }

//             have_fived.insert(j);
//         }
//     }
//     0
// }

// fn part2(pre: &str, mut count: usize) -> usize {
//     let mut five_table: HashMap<_, Vec<_>> = HashMap::new();
//     let mut have_fived = HashSet::new();

//     for i in 0.. {
//         let hh = (0..2016).fold(cal_md5(pre, i), |acc, _| {
//             format!("{:x}", md5::compute(format!("{acc}").as_bytes()))
//         });
//         let a = has_n_pad(&hh, 3);

//         if let Some(aa) = a {
//             // only first
//             match five_table.get(&aa) {
//                 Some(bucket) => {
//                     if let Some(jj) = bucket.iter().find(|&&v| v <= i + 1000 && v > i) {
//                         //dbg!(i);
//                         //dbg!(jj);
//                         //dbg!(&aa);

//                         count -= 1;
//                         if count == 0 {
//                             //dbg!(&aa);
//                             //dbg!(five_table.get(&aa));
//                             return i;
//                         }
//                     }
//                 }
//                 None => (),
//             }
//         }

//         for j in i + 1..=i + 2000 {
//             if have_fived.contains(&j) {
//                 continue;
//             }
//             let hh = (0..2016).fold(cal_md5(pre, j), |acc, _| {
//                 format!("{:x}", md5::compute(format!("{acc}").as_bytes()))
//             });
//             if let Some(aa) = has_n_pad(&hh, 5) {
//                 // if a.len() > 1 {
//                 //     dbg!(&a);
//                 // }
//                 //dbg!(&hh);
//                 five_table.entry(aa).or_insert(vec![]).push(j);
//             }

//             have_fived.insert(j);
//         }
//     }
//     0
// }

fn day14(pre: &str, mut count: usize, part2: bool) -> usize {
    let mut five_table: HashMap<_, Vec<_>> = HashMap::new();
    let mut have_fived = HashSet::new();

    for i in 0.. {
        let hh = if part2 {
            (0..2016).fold(cal_md5(pre, i), |acc, _| cal_md5_2(acc))
        } else {
            cal_md5(pre, i)
        };

        let a = has_n_pad(&hh, 3);

        if let Some(aa) = a {
            match five_table.get(&aa) {
                Some(bucket) => {
                    if let Some(jj) = bucket.iter().find(|&&v| v <= i + 1000 && v > i) {
                        //dbg!(i);
                        //dbg!(jj);
                        //dbg!(&aa);

                        count -= 1;
                        if count == 0 {
                            //dbg!(&aa);
                            //dbg!(five_table.get(&aa));
                            return i;
                        }
                    }
                }
                None => (),
            }
        }

        for j in i + 1..=i + 2000 {
            if have_fived.contains(&j) {
                continue;
            }
            let hh = if part2 {
                (0..2016).fold(cal_md5(pre, j), |acc, _| cal_md5_2(acc))
            } else {
                cal_md5(pre, j)
            };

            if let Some(aa) = has_n_pad(&hh, 5) {
                // if a.len() > 1 {
                //     dbg!(&a);
                // }
                //dbg!(&hh);
                five_table.entry(aa).or_insert(vec![]).push(j);
            }

            have_fived.insert(j);
        }
    }
    0
}

fn main() {
    //dbg!(part1("abc", 64));
    //dbg!(has_n_pad("67e1929c76cbb0c18408c1e154f4f12e", 5));
    //dbg!(vec!['b', '1', '0'].iter().all_equal());

    //dbg!(part1("ihaygndm", 64));
    //dbg!(part2("ihaygndm", 64));

    dbg!(day14("ihaygndm", 64, false));
    dbg!(day14("ihaygndm", 64, true));
}
