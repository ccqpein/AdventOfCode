use tools::*;

fn day5(s: String) -> String {
    let mut res = vec![];
    for n in 1.. {
        let r = format!("{:x}", md5::compute(s.to_string() + &n.to_string()));
        if r[0..5] == *"00000" {
            res.push(r[5..6].to_string());
            if res.len() == 8 {
                break;
            }
        }
    }
    res.join("")
}

fn day5_2(s: String) -> String {
    let mut res = vec![None; 8];
    for n in 1.. {
        let r = format!("{:x}", md5::compute(s.to_string() + &n.to_string()));
        if r[0..5] == *"00000" {
            match r[5..6].parse::<usize>() {
                Ok(n) if n <= 7 => {
                    if res[n].is_none() {
                        res[n] = Some(r[6..7].to_string())
                    }
                }
                _ => (),
            }
            if res.iter().all(|e| e.is_some()) {
                break;
            }
        }
    }
    res.into_iter()
        .map(|e| e.unwrap())
        .collect::<Vec<String>>()
        .join("")
}

fn main() {
    println!("part1: {}", day5("wtnhxymk".to_string()));
    println!("part2: {}", day5_2("wtnhxymk".to_string()));
}
