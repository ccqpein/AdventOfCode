use std::collections::HashSet;

use itertools::Itertools;
use tools::*;

fn word_valid(line: &str) -> bool {
    line.split(' ')
        .into_iter()
        .counts()
        .values()
        .filter(|v| **v != 1)
        .next()
        .is_none()
}

fn day4(inputs: &[String]) -> usize {
    inputs.into_iter().filter(|l| word_valid(l)).count()
}

fn to_set(word: &str) -> HashSet<char> {
    word.chars().collect()
}

fn word_valid_2(line: &str) -> bool {
    let words: Vec<_> = line.split(' ').collect();

    for i in 0..words.len() {
        for j in i + 1..words.len() {
            if words[i].len() == words[j].len() && to_set(words[i]) == to_set(words[j]) {
                return false;
            }
        }
    }
    true
}

fn day4_2(inputs: &[String]) -> usize {
    inputs.into_iter().filter(|l| word_valid_2(l)).count()
}

fn main() {
    let input = read_file_by_line("../inputs/day4.input");

    //dbg!(word_valid("aa bb cc dd ee"));
    dbg!(day4(&input));
    dbg!(day4_2(&input));
}
