use std::collections::{HashMap, HashSet};

use itertools::Itertools;

fn cal_md5(pre: &str, number: usize) -> String {
    format!("{:x}", md5::compute(format!("{pre}{}", number).as_bytes()))
}

fn has_n_pad(s: &str, n: usize) -> Option<Vec<char>> {
    let chars = s.chars().collect::<Vec<_>>();
    let mut result = vec![];
    for i in 0..chars.len().saturating_sub(n - 1) {
        if chars.get(i..i + n).unwrap().iter().all_equal() {
            //dbg!(chars.get(i..i + n));
            //dbg!(chars.get(i..i + n).iter().all_equal_value());
            //dbg!(chars[i] == chars[i + 1]);
            //return Some(vec![chars[i]]);
            result.push(chars[i]);
        }
    }

    if result.len() > 0 {
        if result.len() > 1 {
            // println!("{s} has more than 1 chars dup {n} times: {:?}", &result);
            // println!(
            //     "after dedup: {:?}",
            //     result.iter().sorted().dedup().collect::<Vec<_>>()
            // );
        }
        //Some(result.into_iter().sorted().dedup().collect())
        Some(result)
    } else {
        None
    }
}

fn part1(pre: &str, mut count: usize) -> usize {
    let mut three_table = HashMap::new();
    let mut five_table: HashMap<_, Vec<_>> = HashMap::new();
    let mut have_fived = HashSet::new();

    for i in 0.. {
        let hh = cal_md5(pre, i);
        let a = has_n_pad(&hh, 3);
        match a.as_ref() {
            Some(aa) => {
                for c in aa {
                    three_table.entry(c.clone()).or_insert(vec![]).push(i);
                }
            }
            None => (),
        }

        if let Some(ccs) = a {
            for aa in &ccs[..1] {
                // only first
                match five_table.get(aa) {
                    Some(bucket) => {
                        if let Some(jj) = bucket.iter().find(|&&v| v <= i + 1000 && v > i) {
                            dbg!(i);
                            dbg!(jj);
                            dbg!(&aa);

                            count -= 1;
                            if count == 0 {
                                dbg!(&aa);
                                dbg!(five_table.get(aa));
                                return i;
                            } else {
                                break;
                            }
                        }
                    }
                    None => (),
                }
            }
        }

        for j in i + 1..=i + 2000 {
            if have_fived.contains(&j) {
                continue;
            }
            let hh = cal_md5(pre, j);
            if let Some(a) = has_n_pad(&hh, 5) {
                // if a.len() > 1 {
                //     dbg!(&a);
                // }
                dbg!(&hh);
                dbg!(&a[..1]);
                for aa in a {
                    five_table.entry(aa).or_insert(vec![]).push(j);
                }
            }

            have_fived.insert(j);
        }
    }
    0
}

fn main() {
    dbg!(part1("abc", 64));
    //dbg!(has_n_pad("67e1929c76cbb0c18408c1e154f4f12e", 5));
    //dbg!(vec!['b', '1', '0'].iter().all_equal());

    dbg!(part1("ihaygndm", 64));
}
