use itertools::*;

fn process_input(s: &str) -> String {
    let mut ss = s.chars().collect::<Vec<_>>();
    ss.reverse();
    let mut result = s.chars().collect::<Vec<_>>();
    result.push('0');
    for c in ss {
        result.push(if c == '1' { '0' } else { '1' });
    }

    String::from_iter(result)
}

fn checksum(s: &[char]) -> String {
    let next_round: Vec<char> = s
        .iter()
        .chunks(2)
        .into_iter()
        .map(|mut c| {
            if c.next().unwrap() == c.next().unwrap() {
                '1'
            } else {
                '0'
            }
        })
        .collect();

    if next_round.len() % 2 == 0 {
        checksum(&next_round)
    } else {
        String::from_iter(next_round)
    }
}

fn part1(mut init: String, len: usize) -> String {
    while init.len() < len {
        init = process_input(&init)
    }

    checksum(&init.chars().collect::<Vec<_>>()[..len])
}

fn main() {
    // dbg!(process_input("1"));
    // dbg!(process_input("0"));
    // dbg!(process_input("11111"));
    // dbg!(process_input("111100001010"));

    // dbg!(checksum(&"110010110100".chars().collect::<Vec<_>>()));
    // dbg!(checksum(
    //     &"10000011110010000111".chars().collect::<Vec<_>>()
    // ));

    dbg!(part1("10001001100000001".to_string(), 272));
    dbg!(part1("10001001100000001".to_string(), 35651584));
}
