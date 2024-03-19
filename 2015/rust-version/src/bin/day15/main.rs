use itertools::Itertools;
use regex::Regex;
use tools::*;

fn sump1(data: &[[i32; 5]], bucket: Vec<usize>) -> i32 {
    let mut result = [0; 4];
    for i in 0..4 {
        for (ind, d) in data.iter().enumerate() {
            result[i] += d[i] * (bucket[ind] as i32);
        }
    }
    result
        .into_iter()
        .map(|i| if i < 0 { 0 } else { i })
        .product()
}

fn p1(data: &[[i32; 5]], left: usize, mut bucket: Vec<usize>) -> i32 {
    if data.len() - 1 == bucket.len() {
        bucket.push(left);
        return sump1(data, bucket);
    }

    (1..=left - (data.len() - bucket.len()) + 1)
        .map(|a| {
            let mut bb = bucket.clone();
            bb.push(a);
            p1(data, left - a, bb)
        })
        .max()
        .unwrap()
}

fn sump2(data: &[[i32; 5]], bucket: Vec<usize>) -> i32 {
    let mut result = [0; 5];
    for i in 0..5 {
        for (ind, d) in data.iter().enumerate() {
            result[i] += d[i] * (bucket[ind] as i32);
        }
    }

    if result[4] != 500 {
        return 0;
    }

    result
        .get(0..4)
        .unwrap()
        .into_iter()
        .map(|i| if *i < 0 { 0 } else { *i })
        .product()
}
fn p2(data: &[[i32; 5]], left: usize, mut bucket: Vec<usize>) -> i32 {
    if data.len() - 1 == bucket.len() {
        bucket.push(left);
        return sump2(data, bucket);
    }

    (1..=left - (data.len() - bucket.len()) + 1)
        .map(|a| {
            let mut bb = bucket.clone();
            bb.push(a);
            p2(data, left - a, bb)
        })
        .max()
        .unwrap()
}

fn day15(input: &[String], part1: bool) -> i32 {
    let re = Regex::new(
        r#"^(.+): capacity ([+-]?\d+), durability ([+-]?\d+), flavor ([+-]?\d+), texture ([+-]?\d+), calories ([+-]?\d+)$"#,
    )
    .unwrap();

    let data = input
        .iter()
        .map(|l| {
            dbg!(l);
            let (_, [_, cap, du, fl, te, cal]) = re.captures(l).unwrap().extract();
            [
                cap.parse::<i32>().unwrap(),
                du.parse::<i32>().unwrap(),
                fl.parse::<i32>().unwrap(),
                te.parse::<i32>().unwrap(),
                cal.parse::<i32>().unwrap(),
            ]
        })
        .collect::<Vec<_>>();

    dbg!(&data);
    let bucket = vec![];
    if part1 {
        p1(&data, 100, bucket)
    } else {
        p2(&data, 100, bucket)
    }
}

fn main() {
    let input = r#"Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8"#
        .lines()
        .map(|l| l.to_string())
        .collect::<Vec<_>>();

    // let input = r#"Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
    // Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"#
    //     .lines()
    //     .map(|l| l.to_string())
    //     .collect::<Vec<_>>();

    println!("1: {:?}", day15(&input, true));
    println!("2: {:?}", day15(&input, false))
}
