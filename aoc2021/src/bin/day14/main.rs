use std::collections::HashMap;

use tools::*;

fn part1(input: &Vec<String>, step: usize) -> i32 {
    let mut template = input[0].to_string();
    let mut all_map = input[2..]
        .iter()
        .map(|s| {
            let mut ss = s.split(" -> ");
            (
                ss.next().unwrap().to_string(),
                ss.next().unwrap().to_string(),
            )
        })
        .collect::<HashMap<String, String>>();

    for s in 0..step {
        println!("step {}", s);
        let mut all_insert = vec![];
        for ind in 0..template.len() - 1 {
            all_insert.push(all_map[&template[ind..ind + 2].to_string()].clone());
        }
        let mut new_template = vec![];
        for ind in 0..template.len() - 1 {
            new_template.push(template[ind..ind + 1].to_string());
            new_template.push(all_insert[ind].clone());
        }
        new_template.push(template[template.len() - 1..template.len()].to_string());

        template = String::from_iter(new_template);
    }

    let mut record = HashMap::new();
    template
        .chars()
        .for_each(|c| *record.entry(c).or_insert(0) += 1);

    dbg!(template.len());

    let mut max = 0;
    let mut min = i32::MAX;
    for (k, v) in record {
        max = i32::max(v, max);
        min = i32::min(v, min);
    }

    dbg!(max);
    dbg!(min);
    max - min
}

fn part1_v2(input: &Vec<String>, step: usize) -> i32 {
    let mut template = input[0].to_string();
    let mut all_map = input[2..]
        .iter()
        .map(|s| {
            let mut ss = s.split(" -> ");
            (
                ss.next().unwrap().to_string(),
                ss.next().unwrap().to_string(),
            )
        })
        .collect::<HashMap<String, String>>();

    let mut record = HashMap::new();
    for s in 0..step {
        println!("step {}", s + 1);
        println!("record length {}", record.len());
        let next_temp = fast_match(template.clone(), &mut all_map, &mut record);
        //dbg!(&next_temp);
        update_record(&template, &next_temp, &mut record);
        template = next_temp;
    }
    //dbg!(&template);

    let mut count_record = HashMap::new();
    template
        .chars()
        .for_each(|c| *count_record.entry(c).or_insert(0) += 1);

    dbg!(template.len());

    let mut max = 0;
    let mut min = i32::MAX;
    for (k, v) in count_record {
        max = i32::max(v, max);
        min = i32::min(v, min);
    }

    dbg!(max);
    dbg!(min);
    max - min
}

fn fast_match(
    template: String,
    map: &mut HashMap<String, String>,
    record: &mut HashMap<String, String>,
) -> String {
    if template.len() == 1 {
        return template;
    }

    if template.len() == 2 {
        let result = template[0..1].to_string()
            + &map[&template[0..2].to_string()]
            + &fast_match(template[1..].to_string(), map, record);

        return result;
    }

    let mut cache = vec![];
    let mut offset = 0;
    while offset <= template.len() - 1 {
        if offset == template.len() - 1 {
            cache.push(template[template.len() - 1..template.len()].to_string());
            offset += 1;
        } else if offset + 3 <= template.len() {
            match record.get(&template[offset..offset + 3]).cloned() {
                Some(v) => {
                    cache.push(v[0..v.len() - 1].to_string());
                    offset += 2
                }
                None => {
                    cache.push(
                        template[offset..offset + 1].to_string()
                            + &map[&template[offset..offset + 2].to_string()],
                    );
                    offset += 1;
                }
            }
        } else {
            cache.push(
                template[offset..offset + 1].to_string()
                    + &map[&template[offset..offset + 2].to_string()],
            );
            offset += 1;
        }
    }

    String::from_iter(cache)
}

fn update_record(template: &str, next_temp: &str, record: &mut HashMap<String, String>) {
    for begin in 0..=template.len() - 3 {
        match record.get(&template[begin..begin + 3]) {
            Some(_) => continue,
            None => {
                record.insert(
                    template[begin..begin + 3].to_string(),
                    next_temp[begin * 2..begin * 2 + 5].to_string(),
                );
            }
        }
    }
}

fn day14_v3(input: &Vec<String>, step: usize) -> usize {
    let mut template = input[0].to_string();
    let mut all_map = input[2..]
        .iter()
        .map(|s| {
            let mut ss = s.split(" -> ");
            (
                {
                    let mut cc = ss.next().unwrap().chars();
                    (cc.next().unwrap(), cc.next().unwrap())
                },
                ss.next().unwrap().chars().next().unwrap(),
            )
        })
        .collect::<HashMap<(char, char), char>>();

    let mut counts = template.chars().fold(HashMap::new(), |mut m, c| {
        *m.entry(c).or_insert(0) += 1;
        m
    });

    let mut pairs = HashMap::new();
    (0..=template.len() - 2).into_iter().for_each(|ind| {
        *pairs
            .entry((
                template.chars().nth(ind).unwrap(),
                template.chars().nth(ind + 1).unwrap(),
            ))
            .or_insert(0) += 1
    });

    //dbg!(&pairs.len());
    for s in 0..step {
        //println!("step: {}", s + 1);
        let mut new_pair = HashMap::new();
        for ((a, b), count) in pairs {
            let c = all_map[&(a, b)];
            *counts.entry(c).or_insert(0) += count;

            *new_pair.entry((a, c)).or_insert(0) += count;
            *new_pair.entry((c, b)).or_insert(0) += count;
        }

        pairs = new_pair
    }

    let mut max = 0;
    let mut min = usize::MAX;
    for (_, v) in counts {
        max = usize::max(v, max);
        min = usize::min(v, min);
    }

    max - min
}

fn main() {
    let input = read_file_by_line("./src/bin/day14/day14.input");
    //let input = read_file_by_line("./src/bin/day14/day14_demo.input");
    //println!("{}", part1(&input, 40));
    //println!("{}", part1_v2(&input, 40));
    println!("{}", day14_v3(&input, 40));
}
