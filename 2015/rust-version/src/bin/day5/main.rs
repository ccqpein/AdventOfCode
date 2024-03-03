use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;
use tools::*;

fn nice_string(s: &str) -> bool {
    let mut vowel_count = 0;
    let mut letter_check = '%';
    let mut letter_check_bool = false;

    for c in s.chars() {
        match c {
            'a' | 'e' | 'i' | 'o' | 'u' => vowel_count += 1,
            _ => (),
        }

        match (letter_check, c) {
            ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') => return false,
            _ if letter_check == c => letter_check_bool = true,
            _ => (),
        }
        letter_check = c.clone();
    }

    letter_check_bool && (vowel_count >= 3)
}

fn day5(inputs: &[String]) -> usize {
    inputs.iter().filter(|s| nice_string(s)).count()
}

fn nice_string2(s: &str) -> bool {
    let mut pairs_table = HashMap::new();
    let mut chars_table = HashMap::new();
    //let chars = s.chars();
    for start in 0..s.len() - 1 {
        let entry = pairs_table.entry(&s[start..start + 2]).or_insert(vec![]);
        entry.push(start);

        let entry = chars_table.entry(&s[start..start + 1]).or_insert(vec![]);
        entry.push(start);
    }

    let entry = chars_table
        .entry(&s[s.len() - 1..s.len()])
        .or_insert(vec![]);
    entry.push(s.len() - 1);

    let mut pair_flag = false;
    // start to check
    for (k, v) in pairs_table {
        if v.iter()
            .tuple_windows()
            .map(|(a, b)| b - a)
            .filter(|v| *v > 1)
            .count()
            > 0
        {
            pair_flag = true;
            break;
        }
    }

    let mut char_flag = false;
    for (c, v) in chars_table {
        if v.iter()
            .tuple_windows()
            .map(|(a, b)| b - a)
            .filter(|v| *v == 2)
            .count()
            > 0
        {
            char_flag = true;
            break;
        }
    }

    pair_flag && char_flag
}

fn day5_p2(inputs: &[String]) -> usize {
    inputs.iter().filter(|s| nice_string2(s)).count()
}

fn main() {
    let input = read_file_by_line("../inputs/day5.input");
    //let input = vec!["ugknbfddgicrmopn".to_string()];
    println!("{:?}", day5(&input));

    //let input = vec!["qjhvhtzxzqqjkmpb".to_string()];
    //let input = vec!["xxyxx".to_string()];
    println!("{:?}", day5_p2(&input));
}
