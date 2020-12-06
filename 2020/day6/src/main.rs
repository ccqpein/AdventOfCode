use std::collections::HashSet;
use tools::read_file_by_line;

fn make_set(cache: &Vec<String>) -> HashSet<char> {
    let mut s = HashSet::new();
    cache.iter().for_each(|line| {
        line.chars().for_each(|c| {
            s.insert(c);
            ()
        })
    });
    s
}

fn part1(input: &Vec<String>) -> usize {
    let mut cache = vec![];
    let mut count = 0;
    for line in input {
        if line == "" {
            count += make_set(&cache).len();
            cache.clear();
        } else {
            cache.push(line.clone());
        }
    }
    count + make_set(&cache).len()
}

fn intersect_set(cache: &Vec<String>) -> HashSet<char> {
    let original = "qwertyuiopasdfghjklzxcvbnm".chars().collect::<HashSet<_>>();
    cache
        .iter()
        .map(|line| line.chars().collect::<HashSet<char>>())
        .fold(original.clone(), |acc, x| {
            acc.intersection(&x).cloned().collect()
        })
}

fn part2(input: &Vec<String>) -> usize {
    let mut cache = vec![];
    let mut count = 0;
    for line in input {
        if line == "" {
            count += intersect_set(&cache).len();
            cache.clear();
        } else {
            cache.push(line.clone());
        }
    }
    count + intersect_set(&cache).len()
}

fn main() {
    let input = read_file_by_line(String::from("./src/day6.input"));
    println!("part1: {:?}", part1(&input));
    println!("part2: {:?}", part2(&input));
}
