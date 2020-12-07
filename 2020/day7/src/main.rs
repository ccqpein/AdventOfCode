use std::collections::{HashMap, HashSet};
use tools::read_file_by_line;

// this function is wrong
fn cut_line(s: &str, table: &mut HashMap<String, Vec<(usize, String)>>) {
    let s = s.split_whitespace();
    let mut cache = vec![];
    let mut result = vec![];
    for word in s {
        match word {
            "bags" => {
                result.push(cache.clone());
                cache.clear()
            }
            "bags," | "bags." | "bag." => {
                result.push(cache.clone());
                cache.clear()
            }
            "contain" => {}
            _ => cache.push(word),
        }
    }

    let key = format!("{} {}", result[0][0], result[0][1]);
    //println!("{}, {:?}", key, result);
    let en = table.entry(key).or_insert(vec![]);
    for ind in 1..result.len() {
        if let Ok(count) = result[ind][0].parse::<usize>() {
            let v = format!("{} {}", result[ind][1], result[ind][2]);
            en.push((count, v))
        }
    }
}

fn cut_line2(line: &str, table: &mut HashMap<String, Vec<(usize, String)>>) {
    let mut parts = line.split(" bags contain ");
    let color = parts.next().unwrap().to_string();
    let rules = parts
        .next()
        .unwrap()
        .split(", ")
        .filter_map(|element| {
            let mut words = element.splitn(2, ' ');
            let n = match words.next()? {
                "no" => None,
                n => n.parse::<usize>().ok(),
            }?;
            let inner = words.next()?.rsplitn(2, ' ').skip(1).next()?.to_string();
            (n, inner).into()
        })
        .collect();
    table.insert(color, rules);
}

fn parser(input: &Vec<String>) -> HashMap<String, Vec<(usize, String)>> {
    let mut table: HashMap<String, Vec<(usize, String)>> = HashMap::new();

    input.iter().for_each(|s| cut_line2(s, &mut table));
    //println!("{}", table.len());
    table
}

fn search_map(table: &HashMap<String, Vec<(usize, String)>>, key: &str, mat: &str) -> usize {
    if key == mat {
        return 1;
    };
    let v = table.get(key).unwrap();
    if v.is_empty() {
        return 0;
    }

    v.iter().map(|d| search_map(table, &d.1, mat)).sum()
}

fn part1(table: &HashMap<String, Vec<(usize, String)>>, mat: &str) -> usize {
    let mut count = 0;
    for (k, v) in table.iter() {
        if k == mat {
            continue;
        }
        if search_map(table, k, mat) != 0 {
            count += 1;
        }
    }
    count
}

fn part2(table: &HashMap<String, Vec<(usize, String)>>, mat: &str) -> usize {
    let dd = table.get(mat).unwrap();
    if dd.is_empty() {
        return 1;
    }

    1_usize
        + dd.iter()
            .map(|en| en.0 * part2(table, &en.1))
            .sum::<usize>()
}

fn main() {
    let input = read_file_by_line(String::from("./src/day7.input"));

    //println!("{:?}", parser(&input));
    let table = parser(&input);
    println!("{:?}", part1(&table, "shiny gold"));
    println!("{:?}", part2(&table, "shiny gold") - 1)
}
