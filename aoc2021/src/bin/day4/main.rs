use std::collections::HashSet;

use tools::*;

fn part1(input: &Vec<String>) -> usize {
    let command = input[0]
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    //dbg!(&command);
    let mut all_square = input[1..].chunks(6).collect::<Vec<_>>();
    let mut all_square = all_square
        .into_iter()
        .map(|chunk| {
            chunk
                .iter()
                .filter(|l| *l != "")
                .map(|l| {
                    l.split(|c| c == ' ')
                        .filter(|s| *s != "")
                        .map(|c| c.parse::<usize>().unwrap())
                        .collect::<Vec<usize>>()
                })
                .collect::<Vec<Vec<usize>>>()
        })
        .collect::<Vec<Vec<Vec<usize>>>>();

    //dbg!(&all_square.last().unwrap());

    let mut all_board_set: Vec<HashSet<usize>> = vec![];
    for chunk in &all_square {
        all_board_set.push(chunk[0].iter().cloned().collect::<HashSet<_>>());
        all_board_set.push(chunk[1].iter().cloned().collect::<HashSet<_>>());
        all_board_set.push(chunk[2].iter().cloned().collect::<HashSet<_>>());
        all_board_set.push(chunk[3].iter().cloned().collect::<HashSet<_>>());
        all_board_set.push(chunk[4].iter().cloned().collect::<HashSet<_>>());
        all_board_set.push(chunk.iter().map(|l| l[0]).collect());
        all_board_set.push(chunk.iter().map(|l| l[1]).collect());
        all_board_set.push(chunk.iter().map(|l| l[2]).collect());
        all_board_set.push(chunk.iter().map(|l| l[3]).collect());
        all_board_set.push(chunk.iter().map(|l| l[4]).collect());
    }
    //dbg!(&all_square);
    let mut last: HashSet<usize> = HashSet::new();
    for this in command {
        last.insert(this);
        let mut temp = 0;
        for (ind, set) in all_board_set.iter().enumerate() {
            if set.difference(&last).count() == 0 {
                dbg!(set);
                //dbg!(&last);
                let number = (ind / 10) as usize;

                dbg!(&all_square[number]);
                temp += all_square[number]
                    .iter()
                    .map(|l| {
                        let a = l.iter().filter(|c| !last.contains(c)).sum::<usize>();
                        println!("{}", a);
                        a
                    })
                    .sum::<usize>();

                dbg!(&temp);
                //return set.iter().map(|x| *x).sum::<usize>() * this;
            }
        }
        if temp != 0 {
            return temp * this;
        }
    }

    0
}

struct Squid {
    is_win: bool,
    all_elements: Vec<HashSet<usize>>,
    all_row_col: Vec<HashSet<usize>>,
}

impl Squid {
    fn new(input: &Vec<Vec<usize>>) -> Self {
        let mut all_elements = vec![];
        let mut all_row_col: Vec<HashSet<usize>> = vec![];
        for l in input {
            all_elements.push(HashSet::from_iter(l.clone()));
            all_row_col.push(HashSet::from_iter(l.clone()));
        }

        for i in 0..5 {
            all_row_col.push(HashSet::from_iter(input.iter().map(|l| l[i])));
        }
        Self {
            is_win: false,
            all_elements,
            all_row_col,
        }
    }

    fn is_win(&self, set: &HashSet<usize>) -> bool {
        for line in &self.all_row_col {
            if line.difference(set).count() == 0 {
                return true;
            }
        }
        false
    }

    fn all_unmark_value<'s>(&'s self, set: &'s HashSet<usize>) -> impl Iterator<Item = &'s usize> {
        self.all_elements
            .iter()
            .map(|l| l.difference(set))
            .flatten()
    }
}

fn part2(input: &Vec<String>) -> usize {
    let command = input[0]
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    //dbg!(&command);
    let mut all_square = input[1..].chunks(6).collect::<Vec<_>>();
    let mut all_square = all_square
        .into_iter()
        .map(|chunk| {
            chunk
                .iter()
                .filter(|l| *l != "")
                .map(|l| {
                    l.split(|c| c == ' ')
                        .filter(|s| *s != "")
                        .map(|c| c.parse::<usize>().unwrap())
                        .collect::<Vec<usize>>()
                })
                .collect::<Vec<Vec<usize>>>()
        })
        .collect::<Vec<Vec<Vec<usize>>>>();

    let mut all_square: Vec<Squid> = all_square.iter().map(|chunk| Squid::new(chunk)).collect();

    //----

    let mut last: HashSet<usize> = HashSet::new();
    let mut temp = 0;
    for this in command {
        last.insert(this);
        for sq in all_square.as_mut_slice() {
            if sq.is_win == true {
                continue;
            }
            if sq.is_win(&last) {
                sq.is_win = true;
                temp = sq.all_unmark_value(&last).sum::<usize>() * this
            }
        }
        if all_square.iter().all(|sq| sq.is_win) {
            return temp;
        }
    }
    0
}

fn main() {
    let input = read_file_by_line("./src/bin/day4/day4.input");
    //let input = read_file_by_line("./src/bin/day4/day4_demo.input");
    //println!("{}", part1(&input));
    println!("{}", part2(&input));
}
