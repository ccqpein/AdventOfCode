use std::collections::HashMap;

use tools::*;

enum Entry {
    File(usize),
    Dir(HashMap<String, Entry>),
}

fn day7(input: &Vec<String>) {
    let mut table: HashMap<String, Entry> = HashMap::new();
    let mut tree = vec![];

    table.insert("/".to_string(), Entry::Dir(HashMap::new()));

    for line in input {
        let mut splited_line = line.split(' ');
        match splited_line.next().unwrap() {
            "$" => {
                if splited_line.next().unwrap() == "cd" {
                    let dir_name = splited_line.next().unwrap();
                    if dir_name == ".." {
                        tree.pop();
                    } else {
                        tree.push(dir_name.to_string())
                    }
                }
            }
            "dir" => {
                let sub_dir = splited_line.next().unwrap();
                let mut this_table = &mut table;

                for d in &tree {
                    match this_table.get_mut(d).unwrap() {
                        Entry::File(_) => unreachable!(),
                        Entry::Dir(tt) => this_table = tt,
                    }
                }
                this_table.insert(sub_dir.to_string(), Entry::Dir(HashMap::new()));
            }
            size @ _ => {
                let filename = splited_line.next().unwrap();
                let mut this_table = &mut table;
                for d in &tree {
                    match this_table.get_mut(d).unwrap() {
                        Entry::File(_) => unreachable!(),
                        Entry::Dir(tt) => this_table = tt,
                    }
                }
                this_table.insert(filename.to_string(), Entry::File(size.parse().unwrap()));
            }
        }
    }

    let mut record_table: HashMap<String, usize> = HashMap::new();
    cal_record(&table, &mut record_table, vec![]);

    //*record_table.get("/").unwrap()

    println!("part1: {}", part1(&record_table));
    println!("part2: {}", part2(&record_table));
}

fn cal_record(
    table: &HashMap<String, Entry>,
    record: &mut HashMap<String, usize>,
    prefix: Vec<String>,
) -> usize {
    let mut count = 0;
    for (k, v) in table {
        match v {
            Entry::File(a) => count += a,
            Entry::Dir(sub_dir) => {
                let mut next = prefix.clone();
                next.push(k.to_string());
                count += cal_record(sub_dir, record, next);
            }
        }
    }
    record.insert(prefix.join("->"), count);
    count
}

fn part1(record: &HashMap<String, usize>) -> usize {
    record
        .iter()
        .filter(|(_, v)| *v < &100000_usize)
        .map(|(_, v)| v)
        .sum()
}

fn part2(record: &HashMap<String, usize>) -> usize {
    let all = record.get("/").unwrap();
    let need = 30000000 - (70000000 - all);
    *record
        .iter()
        .filter(|(k, v)| **v >= need)
        .map(|(k, v)| v)
        .min()
        .unwrap()
}

fn main() {
    //let input = read_file_by_line("./inputs/day7_demo.input");
    let input = read_file_by_line("./inputs/day7.input");
    day7(&input);
    //println!("{}", day7(&input));
    //println!("{}", day7_part2(&input));
}
