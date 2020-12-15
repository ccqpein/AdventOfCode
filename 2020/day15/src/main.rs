use std::collections::HashMap;
use tools::read_file_by_line;

fn part1(input: &Vec<usize>, end: usize) -> usize {
    let mut table = HashMap::new();
    let mut count = 0;
    let mut last = 0;

    input.iter().for_each(|d| {
        count += 1;
        let a = table.entry(*d).or_insert(vec![]);
        a.push(count);
        last = *d;
    });

    loop {
        count += 1;
        let mut this;
        let en = table.entry(last).or_insert(vec![]);

        if en.len() == 1 {
            let a = table.entry(0).or_insert(vec![]);
            a.push(count);
            this = 0;
        } else {
            let mut b = en.iter().rev().take(2);
            let tt = b.next().unwrap() - b.next().unwrap();
            let b = table.entry(tt).or_insert(vec![]);
            b.push(count);
            this = tt.clone();
        }

        last = this;

        if count == end {
            return last;
        }
    }
}

fn main() {
    let input = vec![14, 8, 16, 0, 1, 17];
    //let input = vec![0, 3, 6];
    println!("part1: {}", part1(&input, 2020));
    println!("part2: {}", part1(&input, 30000000));
}
