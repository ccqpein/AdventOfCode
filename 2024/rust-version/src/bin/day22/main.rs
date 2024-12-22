use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex},
};

use rayon::prelude::*;
use tools::*;

fn mixing(n: i64, other: i64) -> i64 {
    n ^ other
}

fn pruning(n: i64) -> i64 {
    n % 16777216
}

fn next_number(mut n: i64) -> i64 {
    n = pruning(mixing(n, 64 * n));
    n = pruning(mixing(n, n / 32));
    pruning(mixing(n, 2048 * n))
}

fn part2(input: &[String]) -> i64 {
    let mut table: HashMap<(i64, i64, i64, i64), i64> = HashMap::new();
    let mut set = HashSet::new();
    let (mut aa, mut bb, mut cc, mut dd, mut ee);
    let (mut a, mut b, mut c, mut d, mut e);
    for line in input {
        a = line.parse::<i64>().unwrap();
        b = next_number(a);
        c = next_number(b);
        d = next_number(c);
        e = next_number(d);

        for _ in 4..2000 {
            aa = a % 10;
            bb = b % 10;
            cc = c % 10;
            dd = d % 10;
            ee = e % 10;
            let seq = (bb - aa, cc - bb, dd - cc, ee - dd);
            if !set.contains(&seq) {
                *table.entry(seq).or_insert(0) += ee;
                set.insert(seq);
            }
            a = b;
            b = c;
            c = d;
            d = e;
            e = next_number(e);
        }
        set.clear();
    }
    table.into_values().max().unwrap()
}

/// much slower than original version
fn part2_c(input: &[String]) -> i64 {
    let table: Arc<Mutex<HashMap<(i64, i64, i64, i64), i64>>> =
        Arc::new(Mutex::new(HashMap::new()));

    input.into_par_iter().for_each(|line| {
        let in_table = table.clone();
        let mut set = HashSet::new();
        let mut a = line.parse::<i64>().unwrap();
        let mut b = next_number(a);
        let mut c = next_number(b);
        let mut d = next_number(c);
        let mut e = next_number(d);

        let (mut aa, mut bb, mut cc, mut dd, mut ee);

        for _ in 4..2000 {
            aa = a % 10;
            bb = b % 10;
            cc = c % 10;
            dd = d % 10;
            ee = e % 10;
            let seq = (bb - aa, cc - bb, dd - cc, ee - dd);
            if !set.contains(&seq) {
                *in_table.lock().unwrap().entry(seq).or_insert(0) += ee;
                set.insert(seq);
            }
            a = b;
            b = c;
            c = d;
            d = e;
            e = next_number(e);
        }
    });
    *table.lock().unwrap().values().max().unwrap()
}

fn part2_c2(input: &[String]) -> i64 {
    let mut table: HashMap<(i64, i64, i64, i64), i64> = HashMap::new();

    let all: Vec<Vec<_>> = input
        .into_par_iter()
        .map(|line| {
            // let in_table = table.clone();
            let mut set = HashSet::new();
            let mut a = line.parse::<i64>().unwrap();
            let mut b = next_number(a);
            let mut c = next_number(b);
            let mut d = next_number(c);
            let mut e = next_number(d);

            let (mut aa, mut bb, mut cc, mut dd, mut ee);
            let mut result = Vec::with_capacity(2000);
            for _ in 4..2000 {
                aa = a % 10;
                bb = b % 10;
                cc = c % 10;
                dd = d % 10;
                ee = e % 10;
                let seq = (bb - aa, cc - bb, dd - cc, ee - dd);
                if !set.contains(&seq) {
                    result.push((seq, ee));
                    set.insert(seq);
                }
                a = b;
                b = c;
                c = d;
                d = e;
                e = next_number(e);
            }
            result
        })
        .collect();

    all.into_iter().fold(&mut table, |table, s| {
        s.into_iter().for_each(|(k, v)| {
            *table.entry(k).or_insert(0) += v;
        });
        table
    });

    table.into_values().max().unwrap()
}

fn main() {
    let input = read_file_by_line("../inputs/day22.input");
    //dbg!(part2(&input)); //debug ~3s
    dbg!(part2_c(&input)); // debug ~5.7s
    //dbg!(part2_c2(&input)); //debug ~1.83s
}
