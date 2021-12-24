use std::collections::HashMap;

use rayon::prelude::*;
use tools::*;

fn part1(input: &Vec<String>) {
    let input = input
        .into_iter()
        .map(|l| l.split(" ").collect::<Vec<_>>())
        .collect::<Vec<Vec<_>>>();

    let result: Vec<[u8; 14]> = fourteen_digits_generater_unlazy()
        .par_iter()
        .filter_map(|number_14digit| {
            println!("{:?}", number_14digit);
            let mut value_table = [("w", 0), ("x", 0), ("y", 0), ("z", 0)]
                .into_iter()
                .collect::<HashMap<&str, i64>>();
            let mut count = 0;
            for line in &input {
                match (line.get(0), line.get(1), line.get(2)) {
                    (Some(&"inp"), Some(a), None) => {
                        value_table.insert(*a, number_14digit[count] as i64);
                        count += 1;
                    }
                    (Some(&"add"), Some(a), Some(b)) => {
                        //println!("add {} {}", a, b);
                        let bv = if let Some(bv) = value_table.get(b) {
                            bv.clone()
                        } else {
                            b.parse::<i64>().unwrap()
                        };
                        *value_table.get_mut(*a).unwrap() += bv;
                    }
                    (Some(&"mul"), Some(a), Some(b)) => {
                        //let bv = value_table.get(b).unwrap().clone();
                        //println!("{}", b);
                        //println!("{}", a);
                        let bv = if let Some(bv) = value_table.get(b) {
                            bv.clone()
                        } else {
                            b.parse::<i64>().unwrap()
                        };
                        *value_table.get_mut(*a).unwrap() *= bv;
                    }
                    (Some(&"div"), Some(a), Some(b)) => {
                        //let bv = value_table.get(b).unwrap().clone();
                        *value_table.get_mut(*a).unwrap() /= b.parse::<i64>().unwrap();
                    }
                    (Some(&"mod"), Some(a), Some(b)) => {
                        //let bv = value_table.get(b).unwrap().clone();
                        *value_table.get_mut(*a).unwrap() %= b.parse::<i64>().unwrap();
                    }
                    (Some(&"eql"), Some(a), Some(b)) => {
                        let bv = if let Some(bv) = value_table.get(b) {
                            bv.clone()
                        } else {
                            b.parse::<i64>().unwrap()
                        };
                        let av = value_table.get_mut(*a).unwrap();
                        if *av == bv {
                            *av = 1
                        } else {
                            *av = 0
                        }
                    }
                    _ => {
                        unreachable!()
                    }
                }
            }
            if *value_table.get("z").unwrap() == 0 {
                Some(number_14digit.clone())
            } else {
                None
            }
        })
        .collect();

    for r in result {
        println!("{:?}", r);
    }
}

fn fourteen_digits_generater() -> impl Iterator<Item = [u8; 14]> {
    let mut seed = [9; 14];
    std::iter::from_fn(move || {
        if seed.iter().all(|e| *e == 1) {
            None
        } else {
            for idx in (0..14).rev() {
                if seed[idx] != 1 {
                    seed[idx] -= 1;
                    return Some(seed.clone());
                } else {
                    seed[idx] = 9;
                }
            }
            None
        }
    })
}

fn fourteen_digits_generater_unlazy() -> Vec<[u8; 14]> {
    let mut seed = [9; 14];
    std::iter::from_fn(move || {
        if seed.iter().all(|e| *e == 1) {
            None
        } else {
            for idx in (0..14).rev() {
                if seed[idx] != 1 {
                    seed[idx] -= 1;
                    return Some(seed.clone());
                } else {
                    seed[idx] = 9;
                }
            }
            None
        }
    })
    .collect()
}

fn part1_v2(input: &Vec<String>) {
    let input = input
        .into_iter()
        .map(|l| l.split(" ").collect::<Vec<_>>())
        .collect::<Vec<Vec<_>>>();

    let mut value_table = [
        ("w", "0".to_string()),
        ("x", "0".to_string()),
        ("y", "0".to_string()),
        ("z", "0".to_string()),
    ]
    .into_iter()
    .collect::<HashMap<&str, String>>();

    let mut count = 0;
    let mut cache_table = value_table.clone();
    let mut result = vec![];
    for (idx, line) in input.iter().enumerate() {
        //println!("{:?}", idx);
        match (line.get(0), line.get(1), line.get(2)) {
            (Some(&"inp"), Some(a), None) => {
                //let zz = cache_table.get("z").unwrap().clone();
                result.push(cache_table);
                cache_table = value_table.clone();
                cache_table.insert("z", "last-z".to_string());

                let vv = cache_table.get_mut(*a).unwrap();
                *vv = format!("(nth {} number)", count);
                count += 1;
            }
            (Some(&"add"), Some(a), Some(b)) => {
                let bv = if let Some(bv) = cache_table.get(b) {
                    bv.clone()
                } else {
                    b.to_string()
                };
                let vv = cache_table.get_mut(*a).unwrap();
                *vv = format!("(+ {} {})", vv, bv);
            }
            (Some(&"mul"), Some(a), Some(b)) => {
                let bv = if let Some(bv) = cache_table.get(b) {
                    bv.clone()
                } else {
                    b.to_string()
                };
                let vv = cache_table.get_mut(*a).unwrap();
                *vv = format!("(* {} {})", vv, bv);
            }
            (Some(&"div"), Some(a), Some(b)) => {
                let bv = if let Some(bv) = cache_table.get(b) {
                    bv.clone()
                } else {
                    b.to_string()
                };
                let vv = cache_table.get_mut(*a).unwrap();
                *vv = format!("(/ {} {})", vv, bv);
            }
            (Some(&"mod"), Some(a), Some(b)) => {
                let bv = if let Some(bv) = cache_table.get(b) {
                    bv.clone()
                } else {
                    b.to_string()
                };
                let vv = cache_table.get_mut(*a).unwrap();
                *vv = format!("(mod {} {})", vv, bv);
            }
            (Some(&"eql"), Some(a), Some(b)) => {
                let bv = if let Some(bv) = cache_table.get(b) {
                    bv.clone()
                } else {
                    b.to_string()
                };
                let vv = cache_table.get_mut(*a).unwrap();
                *vv = format!("(if (= {} {}) 1 0)", vv, bv);
            }
            _ => {
                unreachable!()
            }
        }
    }
    result.push(cache_table);
    // if *cache_table.get("z").unwrap() == 0 {
    //     return number_14digit;
    // }

    for (k, v) in result.iter().enumerate() {
        println!("{}: {:?}", k, v.get("z").unwrap());
    }
}

fn main() {
    let input = read_file_by_line("./src/bin/day24/day24.input");
    //let input = read_file_by_line("./src/bin/day24/day24_demo.input");

    // println!(
    //     "{:?}",
    //     fourteen_digits_generater()
    //         .take(10)
    //         .for_each(|e| println!("{:?}", e))
    // );

    //println!("{:?}", fourteen_digits_generater().last().unwrap());
    //println!("{}", 3 / 2);
    // rayon::ThreadPoolBuilder::new()
    //     .num_threads(48)
    //     .build_global()
    //     .unwrap();
    // println!("{:?}", part1(&input))
    println!("{:?}", part1_v2(&input))
}
