use std::{
    collections::{HashMap, LinkedList},
    sync::LazyLock,
};

use regex::Regex;
use tools::*;

#[derive(Debug)]
enum DanceOps {
    Spin(usize),
    Exchange(usize, usize),
    Partner(String, String),
}

static S_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"s([0-9]+)").unwrap());
static E_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"x([0-9]+)/([0-9]+)").unwrap());
static P_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"p(\w)/(\w)").unwrap());

fn parse_input(w: &str) -> DanceOps {
    match (S_RE.captures(w), E_RE.captures(w), P_RE.captures(w)) {
        (None, None, Some(a)) => {
            //dbg!(&a);
            DanceOps::Partner(a[1].to_string(), a[2].to_string())
        }
        (None, Some(a), None) => {
            //dbg!(&a);
            DanceOps::Exchange(a[1].parse().unwrap(), a[2].parse().unwrap())
        }
        (Some(a), None, None) => {
            //dbg!(&a);
            DanceOps::Spin(a[1].parse().unwrap())
        }
        _ => unreachable!(),
    }
}

fn spin(mut s: LinkedList<char>, ind: usize) -> LinkedList<char> {
    let mut a = s.split_off(s.len() - ind);
    a.append(&mut s);
    a
}

fn swap(s: &mut LinkedList<char>, a: usize, b: usize) {
    let aa = a.min(b);
    let bb = a.max(b);

    let mut y = s.split_off(bb);
    let mut x = s.split_off(aa);

    s.push_back(y.pop_front().unwrap());
    y.push_front(x.pop_front().unwrap());

    s.append(&mut x);
    s.append(&mut y);
}

fn swap_p(s: &mut LinkedList<char>, a: String, b: String) {
    let mut aa: usize = 0;
    for (ind, c) in s.iter().enumerate() {
        if c.to_string() == a {
            aa = ind
        }
    }

    let mut bb: usize = 0;
    for (ind, c) in s.iter().enumerate() {
        if c.to_string() == b {
            bb = ind
        }
    }

    swap(s, aa, bb);
}

fn day16(input: &str) -> String {
    let mut re = "abcdefghijklmnop".chars().collect::<LinkedList<_>>();

    for op in input.split(',').map(|w| parse_input(w)) {
        match op {
            DanceOps::Spin(x) => re = spin(re, x),
            DanceOps::Exchange(a, b) => swap(&mut re, a, b),
            DanceOps::Partner(a, b) => swap_p(&mut re, a, b),
        }
    }

    String::from_iter(re.into_iter())
}

fn main() {
    let input = vec!["s1,x3/43,pe/b"];
    let mut a = "abcde".chars().collect::<LinkedList<_>>();
    dbg!(&a);
    for _ in 0..2 {
        a = spin(a, 1);
        dbg!(&a);
        swap(&mut a, 3, 4);
        dbg!(&a);
        swap_p(&mut a, "e".to_string(), "b".to_string());
        dbg!(&a);
    }

    let input = read_file_by_line("../inputs/day16.input");
    dbg!(day16(&input[0]));
}
