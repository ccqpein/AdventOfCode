use std::collections::HashMap;
use tools::read_file_by_line;

fn sort_input(ls: &Vec<String>) -> Vec<u32> {
    let mut a: Vec<u32> = ls.iter().map(|s| s.parse().unwrap()).collect();
    a.sort();
    a
}

fn sort_input2(ls: &Vec<String>) -> Vec<i32> {
    let mut a: Vec<i32> = ls.iter().map(|s| s.parse().unwrap()).collect();
    a.sort();
    a
}

// (1s, 3s)
fn part1_helper<'a>(
    input: u32,
    table: &'a mut HashMap<u32, (u32, u32)>,
    ls: &Vec<u32>,
) -> Option<&'a (u32, u32)> {
    //println!("{}", input);
    if input == 0 {
        return Some(&(0, 0));
    }

    if !ls.contains(&input) {
        return None;
    }

    if table.get(&input).is_some() {
        return table.get(&input);
    }

    let v = if let Some((one, three)) = part1_helper(input - 1, table, ls) {
        (one + 1, *three)
    } else if let Some((one, three)) = part1_helper(input - 3, table, ls) {
        (*one, three + 1)
    } else {
        return None;
    };

    table.insert(input, v);
    table.get(&input)
}

fn part1(input: &Vec<String>) -> u32 {
    let l = sort_input(input);
    //println!("{:?}", l);
    let mut table = HashMap::new();
    let (a, b) = part1_helper(*l.iter().last().unwrap(), &mut table, &l).unwrap();
    a * (b + 1)
}

fn part1_v2_helper(input: &Vec<u32>, find: u32) -> Option<(u32, u32)> {
    if !input.contains(&find) || find < 1 {
        return None;
    }
    if find == 1 {
        return Some((1, 0));
    }

    if let Some((one, three)) = part1_v2_helper(input, find - 1) {
        Some((one + 1, three))
    } else if let Some((one, three)) = part1_v2_helper(input, find - 3) {
        Some((one, three + 1))
    } else {
        None
    }
}

fn part1_v2(input: &Vec<String>) -> u32 {
    let l = sort_input(input);
    let (a, b) = part1_v2_helper(&l, *l.last().unwrap()).unwrap();
    a * (b + 1)
}

fn part2(input: &Vec<String>) -> u64 {
    let l = sort_input2(input);
    //println!("{:?}", l);
    let mut cache: Vec<u64> = vec![0; l.len()];
    cache[l.len() - 1] = 1;

    for ind in (0..l.len()).rev() {
        let this = l[ind];
        for inner_ind in (0..ind).rev() {
            //println!("({},{})", this, l[inner_ind]);
            if l[inner_ind] < this - 3 {
                break;
            }
            cache[inner_ind] += cache[ind]
        }
    }

    //println!("{:?}", cache);
    cache[0] + cache[1] + cache[2]
}

fn main() {
    let input = read_file_by_line(String::from("./src/day10.input"));
    println!("part1: {}", part1(&input));
    println!("part1_v2: {}", part1_v2(&input));

    println!("part2: {}", part2(&input));
}
