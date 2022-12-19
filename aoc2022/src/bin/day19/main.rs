#![feature(let_chains)]

use tools::*;

#[derive(Debug, Clone)]
enum Cost {
    Ore,      // ind 0
    Clay,     // ind 1
    Obsidian, // ind 2
    Geode,    // ind 3
}

impl Cost {
    fn to_ind(&self) -> usize {
        match self {
            Cost::Ore => 0,
            Cost::Clay => 1,
            Cost::Obsidian => 2,
            Cost::Geode => 3,
        }
    }
}

impl From<&'_ str> for Cost {
    fn from(value: &'_ str) -> Self {
        match value {
            "ore" => Cost::Ore,
            "clay" => Cost::Clay,
            "obsidian" => Cost::Obsidian,
            "geode" => Cost::Geode,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
struct Robot {
    get: Cost,
    cost: Vec<(usize, Cost)>,
}

impl Robot {
    // make this robot and return with the new bucket
    fn make_from_bucket(&self, bucket: &[usize; 4]) -> Option<(Self, [usize; 4])> {
        if self.cost.iter().all(|(n, c)| bucket[c.to_ind()] >= *n) {
            let mut new_bucket = bucket.clone();
            self.cost
                .iter()
                .for_each(|(n, c)| new_bucket[c.to_ind()] -= n);
            return Some((self.clone(), new_bucket));
        }

        None
    }
}

fn parse_input(input: &[String]) -> Vec<Vec<Robot>> {
    let mut result = vec![];
    for line in input {
        if line == "" {
            continue;
        }

        let mut this_blue_p = vec![];

        let mut ss = line.split(&['.', ':'][..]);
        ss.next();
        for b in ss {
            if b == "" {
                continue;
            }
            let mut this_bot = Robot {
                get: Cost::Ore,
                cost: vec![],
            };
            let mut bb = b.split(' ');
            //println!("{:?}", bb.clone().collect::<Vec<_>>());
            while let Some(w) = bb.next() {
                match w {
                    "Each" => {
                        this_bot.get = Cost::from(bb.next().unwrap());
                    }
                    "costs" => this_bot.cost.push((
                        bb.next().unwrap().parse::<usize>().unwrap(),
                        Cost::from(bb.next().unwrap()),
                    )),
                    "and" => this_bot.cost.push((
                        bb.next().unwrap().parse::<usize>().unwrap(),
                        Cost::from(bb.next().unwrap()),
                    )),
                    _ => {}
                }
            }
            this_blue_p.push(this_bot)
        }

        result.push(this_blue_p);
    }
    result
}

fn find_the_most(
    blue_p: Vec<Vec<Robot>>,
    running_bots: Vec<Robot>,
    bucket: [usize; 4],
    min_left: usize,
) {
    let mut bucket = bucket;
    // spending

    // gain
    for r in running_bots {
        //bucket[r.get.to_ind()] += 1 // should be new bucket
    }
}

fn main() {
    let input = read_file_by_line("./inputs/day19_demo.input");
    println!("{:?}", parse_input(&input));
}
