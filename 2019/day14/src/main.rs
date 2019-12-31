use num::integer::*;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct Unit {
    element: String,
    num: i32,
}

impl Unit {
    fn new() -> Self {
        Unit {
            element: String::new(),
            num: 0,
        }
    }

    fn parse(unit: &str) -> Self {
        // parse "333 ssss"
        let temp = unit.split(' ').filter(|b| *b != "").collect::<Vec<&str>>();
        let mut result = Self::new();
        if let Ok(n) = temp[0].parse::<i32>() {
            result.num = n
        } else {
            panic!("cannot parse");
        }

        result.element = temp[1].to_string();
        result
    }
}

#[derive(Debug, Hash)]
struct ReactExpression {
    source_list: Vec<(String, i64)>,
    result: (String, i64),
}

impl ReactExpression {
    fn new() -> Self {
        ReactExpression {
            source_list: vec![],
            result: (String::new(), 0),
        }
    }

    fn parse(line: &str) -> Self {
        let mut result = Self::new();
        let mut temp: Vec<&str> = line.rsplit(" => ").collect();

        let cache = Unit::parse(temp[0]);
        result.result = (cache.element, cache.num as i64);

        temp = temp[1].split(", ").collect();
        for cache in temp.iter().map(|x| Unit::parse(x)) {
            result.source_list.push((cache.element, cache.num as i64));
        }
        result
    }

    fn all_source(&self) -> Vec<(String, i64)> {
        self.source_list.clone()
    }

    fn is_root(&self) -> bool {
        if self.source_list.len() == 1 && self.source_list[0].0 == "ORE" {
            true
        } else {
            false
        }
    }
}

struct ReactChain {
    table: HashMap<String, ReactExpression>,
    depth: HashMap<String, i32>,

    refuse: HashMap<String, i64>,
}

impl ReactChain {
    fn new(rs: Vec<ReactExpression>) -> Self {
        let mut table = HashMap::new();
        let mut depth = HashMap::new();
        let mut refuse = HashMap::new();

        depth.insert("ORE".to_string(), 0);
        for r in rs {
            if r.is_root() {
                depth.insert(r.result.0.clone(), 1);
            }
            refuse.insert(r.result.0.clone(), 0);
            table.insert(r.result.0.clone(), r);
        }

        Self {
            table: table,
            depth: depth,
            refuse: refuse,
        }
    }

    fn update_depth(&mut self) {
        let all_set = self
            .table
            .keys()
            .map(|k| k.clone())
            .collect::<HashSet<String>>();
        let mut already_added_set: HashSet<String> = self.depth.keys().map(|x| x.clone()).collect();

        let mut next_round: Vec<String> = all_set
            .difference(&already_added_set)
            .map(|x| x.clone())
            .collect();

        while next_round.len() != 0 {
            //println!("this is next round {:?}", next_round);
            for s in next_round {
                let mut temp_list = self
                    .table
                    .get(&s)
                    .unwrap()
                    .all_source()
                    .iter()
                    .map(|ss| {
                        if already_added_set.contains(&ss.0) {
                            self.depth.get(&ss.0).unwrap() + 1
                        } else {
                            100
                        }
                    })
                    .collect::<Vec<i32>>();

                temp_list.sort();
                if temp_list[0] == 100 {
                    continue;
                }
                self.depth.insert(s.to_string(), temp_list[0]);
                already_added_set = self.depth.keys().map(|x| x.clone()).collect();
            }
            next_round = all_set
                .difference(&already_added_set)
                .map(|x| x.clone())
                .collect()
        }
    }

    fn find_sum_ele(&self, result: &str, many: i64) -> Vec<(String, i64)> {
        let re = if let Some(r) = self.table.get(result) {
            r
        } else {
            panic!(result.to_string());
        };

        let total = if many <= re.result.1 {
            1
        } else {
            if many % re.result.1 != 0 {
                (many / re.result.1 + 1)
            } else {
                many / re.result.1
            }
        };
        //dbg!(&total);
        re.all_source()
            .iter()
            .map(|x| (x.0.clone(), x.1 * total))
            .collect::<Vec<(String, i64)>>()
    }

    // this function is totaly wrong
    fn find_to_ORE(&self, result: &str, many: i64) -> Vec<(String, i64)> {
        let mut list: Vec<(String, i64)> = self.find_sum_ele(result, many);

        let mut highest_root = list
            .iter()
            .map(|x| self.depth.get(&x.0).unwrap())
            .max()
            .unwrap();
        while *highest_root != 1 {
            println!("this is list {:?}", list);
            let mut temp: Vec<Vec<(String, i64)>> = list
                .iter()
                .map(|x| {
                    if self.depth.get(&x.0).unwrap() == highest_root {
                        self.find_sum_ele(&x.0, x.1)
                    } else {
                        vec![(x.0.clone(), x.1)]
                    }
                })
                .collect();

            //println!("{:?}", &temp);

            let mut table: HashMap<String, i64> = HashMap::new(); // flat list
            for i in temp {
                i.iter()
                    .for_each(|e| *table.entry(e.0.clone()).or_insert(0) += e.1)
            }
            //dbg!(&table);
            list = table.into_iter().collect::<Vec<(String, i64)>>();
            highest_root = list
                .iter()
                .map(|x| self.depth.get(&x.0).unwrap())
                .max()
                .unwrap();
        }

        //list[0].clone()
        while list.len() > 1 {
            let mut temp: Vec<Vec<(String, i64)>> = list
                .iter()
                .map(|x| {
                    if x.0 != "ORE" {
                        self.find_sum_ele(&x.0, x.1)
                    } else {
                        vec![(x.0.clone(), x.1)]
                    }
                })
                .collect();

            println!("{:?}", &temp);

            let mut table: HashMap<String, i64> = HashMap::new(); // flat list
            for i in temp {
                i.iter()
                    .for_each(|e| *table.entry(e.0.clone()).or_insert(0) += e.1)
            }
            //dbg!(&table);
            list = table.into_iter().collect::<Vec<(String, i64)>>();
        }

        list
    }

    fn cost(&mut self, result: &str, many: i64) -> i64 {
        if result == "ORE" {
            return many;
        }

        if *self.refuse.get(result).unwrap() > 0 {
            if *self.refuse.get(result).unwrap() >= many {
                self.refuse
                    .insert(result.to_string(), self.refuse.get(result).unwrap() - many);
                return 0;
            }

            let many = many - self.refuse.get(result).unwrap();
            self.refuse.insert(result.to_string(), 0);
            return self.cost(result, many);
        }

        let quantity_producted = self.table.get(result).unwrap().result.1;
        let list = self.table.get(result).unwrap().all_source();
        let need = (many - 1) / quantity_producted + 1;

        let ore_needed = list.iter().map(|x| self.cost(&x.0, x.1 * need)).sum();

        if need * quantity_producted - many > 0 {
            *self.refuse.get_mut(result).unwrap() += need * quantity_producted - many;
        }
        return ore_needed;
    }

    fn part2(&mut self) -> i64 {
        let stock = 100000;
        let trillion: i64 = 1000000000000;

        let (mut ore_consume, mut fuel, mut last_stock) = (0, 0, 0);
        while ore_consume < trillion {
            last_stock = trillion - ore_consume;
            ore_consume += self.cost("FUEL", stock);
            fuel += 1;
        }

        fuel = (fuel - 1) * stock;

        ore_consume = 0;
        while ore_consume < last_stock {
            ore_consume += self.cost("FUEL", 1);
            fuel += 1
        }

        (fuel - 1) as i64
    }
}

fn read_file(filepath: &str) -> Vec<String> {
    use std::fs::File;
    use std::io::prelude::*;
    use std::io::BufReader;
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

fn main() {
    // println!(
    //     "{:?}",
    //     ReactExpression::parse("20 JVDKQ, 2 LSQFK, 8 SDNCK, 1 MQJNV, 13 LBTV, 3 KPBRX => 5 QBPC")
    // );

    use std::env;

    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();
    let a = read_file(&format!("{}{}", path, "/src/day14.input"));
    let mut rc = ReactChain::new(
        a.iter()
            .map(|x| ReactExpression::parse(x))
            .collect::<Vec<ReactExpression>>(),
    );

    dbg!(rc.cost("FUEL", 1)); // part 1

    let mut rc = ReactChain::new(
        a.iter()
            .map(|x| ReactExpression::parse(x))
            .collect::<Vec<ReactExpression>>(),
    );
    dbg!(rc.part2());
}
