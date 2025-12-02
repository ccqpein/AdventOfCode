use itertools::Itertools;
use regex::Regex;
use tools::*;

fn parse_input(input: &[String]) -> Vec<(String, String)> {
    let re = Regex::new(r"(\d+)-(\d+)").unwrap();
    input[0]
        .split(',')
        .into_iter()
        .map(|a| {
            let Some((_, [start, end])) = re.captures(a).map(|c| c.extract()) else {
                panic!()
            };
            (start.to_string(), end.to_string())
        })
        .collect::<Vec<_>>()
}

fn cut_them(s: &str, len: usize) -> Vec<String> {
    s.chars()
        .chunks(len)
        .into_iter()
        .map(|chunk| chunk.collect::<String>())
        .collect()
}

fn day2(input: &[String]) -> usize {
    let a = parse_input(input);
    let all = a
        .into_iter()
        .map(|(start, end)| {
            (start.parse::<usize>().unwrap()..=end.parse().unwrap()).map(|n| n.to_string())
        })
        .flatten()
        .collect::<Vec<_>>();
    //dbg!(&all);

    all.into_iter()
        .filter_map(|s| {
            if s.len() % 2 == 0 {
                let chunk_len = s.len() / 2;
                //dbg!(chunk_len);
                if cut_them(&s, chunk_len).iter().all_equal() {
                    Some(s.parse::<usize>().unwrap())
                } else {
                    Some(0)
                }
            } else {
                None
            }
        })
        .sum::<usize>()
}

fn check_invalid_id_2(s: &str) -> Option<usize> {
    for chunk_len in 1..s.len() {
        if s.len() % chunk_len == 0 {
            if cut_them(&s, chunk_len).iter().all_equal() {
                return Some(s.parse::<usize>().unwrap());
            }
        }
    }
    None
}

fn day2_2(input: &[String]) -> usize {
    let a = parse_input(input);
    let all = a
        .into_iter()
        .map(|(start, end)| {
            (start.parse::<usize>().unwrap()..=end.parse().unwrap()).map(|n| n.to_string())
        })
        .flatten()
        .collect::<Vec<_>>();
    //dbg!(&all);

    all.into_iter()
        .filter_map(|s| check_invalid_id_2(&s))
        .sum::<usize>()
}

fn main() {
    let input = read_file_by_line("../inputs/day2.input");
    //let input = read_file_by_line("../inputs/day2_demo.input");

    dbg!(day2(&input));
    dbg!(day2_2(&input));
}
