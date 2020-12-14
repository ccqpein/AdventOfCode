#![feature(str_split_once)]
use std::collections::HashMap;
use std::iter::FromIterator;
use tools::read_file_by_line;

fn return_bits(s: &str) -> (u128, u128) {
    let mut flag = false;
    let mut cache = vec![];
    for c in s.chars() {
        if flag {
            cache.push(c);
            continue;
        }
        if c != 'X' {
            cache.push(c);
            flag = true;
        }
    }

    let s = String::from_iter(&cache);
    //println!("1: {}", s.replace("X", "0"));
    //println!("0: {}", s.replace("X", "1"));

    (
        u128::from_str_radix(&s.replace("X", "0"), 2).unwrap(),
        u128::from_str_radix(&s.replace("X", "1"), 2).unwrap(),
    )
}

fn part1(input: &Vec<String>) -> u128 {
    let mut table: HashMap<u64, u128> = HashMap::new();
    let (mut mask1, mut mask0) = (0, 0);
    for line in input {
        if line.starts_with("mask = ") {
            let (_, x) = line.split_at(7);
            let (a, b) = return_bits(x);
            mask1 = a;
            mask0 = b;
            //println!("mask1: {}, mask0 : {}", mask1, mask0);
        } else {
            let mut ss = line.split("] = ");
            let (x, b) = (ss.next().unwrap(), ss.next().unwrap());
            let (_, a) = x.split_at(4);
            //println!("a: {}", a);
            //println!("b: {}", b);
            //println!("b parse: {}", b.parse::<u128>().unwrap() | mask1 & mask0);
            table.insert(
                a.parse().unwrap(),
                (b.parse::<u128>().unwrap() | mask1) & mask0,
            );
        }
    }

    let mut result = 0;
    for v in table.values() {
        //println!("v: {}", v);
        result += v
    }
    result
}

fn mask_this(mask: &str, num: u128) -> String {
    let num_c = format!("{:064b}", num);

    //let mut mask_c = mask.chars();
    let mut mask_c = mask.chars().rev();

    let num_len = num_c.chars().count();
    let mut cache: Vec<char> = vec![];

    for ind in (0..num_len).rev() {
        if let Some(m) = mask_c.next() {
            if m == 'X' {
                cache.push('X');
            } else if m == '1' {
                cache.push('1');
            } else {
                cache.push(num_c.chars().nth(ind).unwrap())
            }
        } else {
            cache.push(num_c.chars().nth(ind).unwrap());
        }
    }

    cache.reverse();
    String::from_iter(cache.iter())
}

fn update_table(table: &mut HashMap<u128, u128>, masked_num: String, value: u128) {
    if let Some((head, tail)) = masked_num.split_once('X') {
        update_table(
            table,
            {
                let mut s: String = head.to_owned();
                s.push_str("1");
                s.push_str(tail);
                s
            },
            value,
        );
        update_table(
            table,
            {
                let mut s: String = head.to_owned();
                s.push_str("0");
                s.push_str(tail);
                s
            },
            value,
        )
    } else {
        table.insert(masked_num.parse::<u128>().unwrap(), value);
    }
}

fn part2(input: &Vec<String>) -> u128 {
    let mut table: HashMap<u128, u128> = HashMap::new();
    let mut mask = "";
    for line in input {
        if line.starts_with("mask = ") {
            mask = line.split_at(7).1;
        } else {
            let mut ss = line.split("] = ");
            let (x, b) = (ss.next().unwrap(), ss.next().unwrap());
            let (_, a) = x.split_at(4);
            update_table(
                &mut table,
                mask_this(mask, a.parse().unwrap()),
                b.parse().unwrap(),
            );
        }
    }

    table.values().sum()
}

fn main() {
    let input = read_file_by_line("./src/day14.input");
    println!("part1: {:?}", part1(&input));

    // println!(
    //     "11: {}",
    //     (11 | "1000000".parse::<u128>().unwrap()) & 0b1111101
    // );
    // println!("0: {}", (0 | 0b1000000) & 0b1111101)

    //println!("{}", mask_this("X1001X", 42));
    println!("part2: {:?}", part2(&input));
}
