use std::collections::HashSet;

use tools::*;

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

fn part1(input: &Vec<String>) -> usize {
    let command = input[0]
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    //dbg!(&command);
    let mut all_square: Vec<Squid> = input[1..]
        .chunks(6)
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
        .map(|chunk| Squid::new(&chunk))
        .collect();

    //-----------------------

    let mut last: HashSet<usize> = HashSet::new();
    for this in command {
        last.insert(this);
        for sq in all_square.as_mut_slice() {
            if sq.is_win(&last) {
                return sq.all_unmark_value(&last).sum::<usize>() * this;
            }
        }
    }

    0
}

fn part2(input: &Vec<String>) -> usize {
    let command = input[0]
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    //dbg!(&command);
    let mut all_square: Vec<Squid> = input[1..]
        .chunks(6)
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
        .map(|chunk| Squid::new(&chunk))
        .collect();

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
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
