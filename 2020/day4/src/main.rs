use std::collections::{HashMap, HashSet};
use tools::read_file_by_line;

fn lines_to_chunk(ls: Vec<String>) -> Vec<Vec<String>> {
    let mut cache = vec![];
    let mut result = vec![];

    for line in ls {
        if &line == "" {
            result.push(cache.clone());
            cache.clear();
        } else {
            cache.push(line);
        }
    }
    if cache.len() != 0 {
        result.push(cache.clone());
    }
    result
}

fn clean_passp(pass: &Vec<String>) -> Vec<String> {
    pass.iter()
        .map(|pass| pass.split_whitespace().collect::<Vec<&str>>())
        .flatten()
        .map(|s| s.to_string())
        .collect()
}

fn split_colon_set<'a>(pass: &'a Vec<String>) -> HashSet<String> {
    pass.iter()
        .map(|p| p.split(|s| s == ':').next().unwrap().to_string())
        .collect()
}

fn split_colon_map(pass: Vec<String>) -> HashMap<String, String> {
    let mut result = HashMap::new();
    pass.iter().for_each(|p| {
        let mut aa = p.split(|s| s == ':');
        result.insert(
            aa.next().unwrap().to_string(),
            aa.next().unwrap().to_string(),
        );
    });
    result
}

fn check_valid(pass: &HashSet<String>) -> bool {
    for field in ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].iter() {
        if let None = pass.get(*field) {
            return false;
        }
    }
    true
}

fn part1(passports: Vec<HashSet<String>>) -> usize {
    passports
        .iter()
        .filter(|passport| check_valid(passport))
        .count()
}

fn part2_checker1(ma: &HashMap<String, String>) -> bool {
    if let Some(v) = ma.get("byr") {
        if let Ok(vv) = v.parse::<u32>() {
            if vv < 1920 || vv > 2002 {
                return false;
            }
        } else {
            return false;
        }
    } else {
        return false;
    }

    if let Some(v) = ma.get("iyr") {
        if let Ok(vv) = v.parse::<u32>() {
            if vv < 2010 || vv > 2020 {
                return false;
            }
        } else {
            return false;
        }
    } else {
        return false;
    }

    if let Some(v) = ma.get("eyr") {
        if let Ok(vv) = v.parse::<u32>() {
            if vv < 2020 || vv > 2030 {
                return false;
            }
        } else {
            return false;
        }
    } else {
        return false;
    }

    true
}

fn check_height(h: &str) -> bool {
    let l = h.len();
    let (n, d) = h.split_at(l - 2);
    let n = if let Ok(a) = n.parse::<u32>() {
        a
    } else {
        return false;
    };
    if d == "cm" {
        if n >= 150 && n <= 193 {
            return true;
        }
    } else {
        if n >= 59 && n <= 76 {
            return true;
        }
    }
    false
}

fn check_hair(h: &str) -> bool {
    if h.len() != 7 {
        return false;
    }

    if !h.starts_with("#") {
        return false;
    }

    for i in 1..7 {
        if let Ok(_) = h[i..i + 1].parse::<u32>() {
            continue;
        }
        let b = h[i..i + 1].as_bytes()[0];
        if b < 97 && b > 102 {
            return false;
        }
    }
    true
}

fn check_eye(e: &str) -> bool {
    ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        .iter()
        .find(|&s| *s == e)
        .is_some()
}

fn check_pid(p: &str) -> bool {
    (p.len() == 9) && p.parse::<u32>().is_ok()
}

fn part2_checker2(ma: &HashMap<String, String>) -> bool {
    if let Some(v) = ma.get("hgt") {
        if !check_height(v) {
            return false;
        }
    } else {
        return false;
    }

    //println!("pass hgt");

    if let Some(v) = ma.get("hcl") {
        if !check_hair(v) {
            return false;
        }
    } else {
        return false;
    }
    //println!("pass hcl");

    if let Some(v) = ma.get("ecl") {
        if !check_eye(v) {
            return false;
        }
    } else {
        return false;
    }
    //println!("pass ecl");

    if let Some(v) = ma.get("pid") {
        if !check_pid(v) {
            return false;
        }
    } else {
        return false;
    }
    //println!("pass pid");

    true
}

fn part2(passports: Vec<HashMap<String, String>>) -> usize {
    passports
        .iter()
        .filter(|pass| {
            // println!("checker1: {}", part2_checker1(pass));
            // println!("checker2: {}", part2_checker2(pass));
            part2_checker1(pass) && part2_checker2(pass)
        })
        .count()
}

fn main() {
    let input = read_file_by_line(String::from("./src/day4.input"));
    let passports = lines_to_chunk(input);

    //println!("{:?}", passports);
    // for i in 0..4 {
    //     println!("{:?}", passports[i]);
    // }

    // println!("{:?}", clean_passp(&passports[0]));

    // println!("{:?}", split_colon(&clean_passp(&passports[0])));
    let a = passports
        .iter()
        .map(|pass| split_colon_set(&clean_passp(pass)))
        .collect::<Vec<HashSet<_>>>();
    println!("{:?}", part1(a));

    let b = passports
        .iter()
        .map(|pass| split_colon_map(clean_passp(pass)))
        .collect::<Vec<HashMap<_, _>>>();
    println!("{:?}", part2(b))
}
