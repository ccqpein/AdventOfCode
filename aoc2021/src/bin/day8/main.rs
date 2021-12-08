use std::collections::HashSet;

use tools::*;

fn part1(input: &Vec<String>) -> usize {
    let content = input
        .iter()
        .map(|l| l.split(" ").filter(|s| *s != "|").collect())
        .collect::<Vec<Vec<&str>>>();

    content
        .iter()
        .map(|l| {
            l[10..l.len()]
                .iter()
                .filter(|w| {
                    let count = w.chars().count();
                    if count == 7 || count == 4 || count == 2 || count == 3 {
                        true
                    } else {
                        false
                    }
                })
                .count()
        })
        .sum()
}

fn part2(input: &Vec<String>) -> usize {
    let content = input
        .iter()
        .map(|l| l.split(" ").filter(|s| *s != "|").collect())
        .collect::<Vec<Vec<&str>>>();
    let mut bucket = vec![];
    for c in content {
        let l_set = parse(&c[0..10]);

        l_set
            .iter()
            .map(|s| String::from_iter(s.iter()))
            .enumerate()
            .for_each(|(k, a)| println!("{}: {}", k, a));

        bucket.push(String::from_iter(
            c[10..c.len()]
                .iter()
                .filter_map(|w| {
                    let wset: HashSet<char> = w.chars().collect();
                    for (ind, s) in l_set.iter().enumerate() {
                        if wset.len() == s.len() && s.difference(&wset).count() == 0 {
                            return Some(ind.to_string());
                        }
                    }
                    None
                })
                .collect::<Vec<String>>(),
        ));
    }

    bucket.iter().map(|s| s.parse::<usize>().unwrap()).sum()
}

fn parse(commands: &[&str]) -> [HashSet<char>; 10] {
    let mut result = vec![HashSet::new(); 7]; // char order 7 no x in input
    let commands = commands
        .iter()
        .map(|d| d.chars().collect::<HashSet<_>>())
        .collect::<Vec<_>>();

    let seven = commands.iter().filter(|s| s.len() == 3).next().unwrap();

    let one = commands.iter().filter(|s| s.len() == 2).next().unwrap();

    let four = commands.iter().filter(|s| s.len() == 4).next().unwrap();

    let eight = commands.iter().filter(|s| s.len() == 7).next().unwrap();

    result[0] = seven.difference(&one).cloned().collect::<HashSet<char>>();

    let six = commands
        .iter()
        .filter(|s| eight.difference(s).count() == 1) // 9/6/0
        .filter(|s| seven.difference(&s).count() == 1)
        .next()
        .unwrap()
        .clone(); // 6

    //println!("6 {}", String::from_iter(six.iter()));

    let nine = commands
        .iter()
        .filter(|s| {
            s.difference(&seven.union(four).cloned().collect::<HashSet<char>>())
                .count()
                == 1
                && s.len() == 6
        })
        .next()
        .unwrap();

    //println!("9 {}", String::from_iter(nine.iter()));

    result[4] = eight.difference(&nine).cloned().collect::<HashSet<char>>();

    result[6] = nine
        .difference(&seven.union(four).cloned().collect())
        .cloned()
        .collect::<HashSet<char>>();

    let six_zero = commands
        .iter()
        .filter(|s| eight.difference(s).count() == 1) // 9/6/0
        .filter(|s| s.difference(&nine).count() != 0); // 6/0

    let zero = six_zero
        .clone()
        .filter(|s| seven.difference(s).count() == 0)
        .next()
        .unwrap();

    result[3] = eight.difference(&zero).cloned().collect();

    let three = seven
        .union(
            &result[3]
                .union(&result[6])
                .cloned()
                .collect::<HashSet<char>>(),
        )
        .cloned()
        .collect::<HashSet<char>>();

    let five: HashSet<char> = six.difference(&result[4]).cloned().collect();

    let two: HashSet<char> = commands
        .iter()
        .filter(|s| eight.difference(s).count() == 2) // 2/3/5
        .filter(|s| s.difference(&nine).count() != 0) // 2/3
        .filter(|s| one.difference(s).count() != 0)
        .next()
        .unwrap()
        .clone();

    [
        zero.clone(),
        one.clone(),
        two,
        three,
        four.clone(),
        five,
        six,
        seven.clone(),
        eight.clone(),
        nine.clone(),
    ]
}

fn main() {
    let input = read_file_by_line("./src/bin/day8/day8.input");
    //let input = read_file_by_line("./src/bin/day8/day8_demo.input");
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
