use std::collections::HashMap;

use regex::Regex;
use tools::*;

fn parse_input(l: &str) -> (&str, &str) {
    let re = Regex::new(r"^(.+) -> (.+)").unwrap();
    let (_, [comm, target]) = re.captures(l).unwrap().extract();
    (comm, target)
}

fn helper<'a>(
    tree_table: &mut HashMap<&'a str, &'a str>,
    v_table: &mut HashMap<&'a str, u16>,
    start: &'a str,
) -> u16 {
    if let Some(vv) = v_table.get(start) {
        return *vv;
    }

    if let Ok(vv) = start.parse::<u16>() {
        return vv;
    }

    let rex: Vec<(usize, Regex)> = vec![
        (1, Regex::new(r"^([a-zA-Z]+)$").unwrap()),
        (2, Regex::new(r"^(.+) AND (.+)$").unwrap()),
        (3, Regex::new(r"^(.+) OR (.+)$").unwrap()),
        (4, Regex::new(r"^(.+) LSHIFT (\d+)$").unwrap()),
        (5, Regex::new(r"^(.+) RSHIFT (\d+)$").unwrap()),
        (6, Regex::new(r"^NOT (.+)$").unwrap()),
        (7, Regex::new(r"^(\d+)$").unwrap()),
    ];

    //println!("start: {:?}", start);

    for (i, r) in rex {
        if let Some(cap) = r.captures(tree_table.get(start).unwrap()) {
            //println!("i: {:?}", i);
            match i {
                1 => {
                    let (_, [x]) = cap.extract();
                    let a = helper(tree_table, v_table, x);
                    v_table.insert(start, a);
                }
                2 => {
                    let (_, [x, y]) = cap.extract();
                    let a = helper(tree_table, v_table, x) & helper(tree_table, v_table, y);
                    v_table.insert(start, a);
                }
                3 => {
                    let (_, [x, y]) = cap.extract();
                    let a = helper(tree_table, v_table, x) | helper(tree_table, v_table, y);
                    v_table.insert(start, a);
                }
                4 => {
                    let (_, [x, y]) = cap.extract();
                    //*v_table.get_mut(start).unwrap() = v_table[x] << y.parse::<u16>().unwrap()
                    let a = helper(tree_table, v_table, x);
                    v_table.insert(start, a << y.parse::<u16>().unwrap());
                }
                5 => {
                    let (_, [x, y]) = cap.extract();
                    //*v_table.get_mut(start).unwrap() = v_table[x] >> y.parse::<u16>().unwrap()
                    let a = helper(tree_table, v_table, x);
                    v_table.insert(start, a >> y.parse::<u16>().unwrap());
                }
                6 => {
                    // *v_table.get_mut(start).unwrap() =
                    //     !*v_table.get(cap.get(0).unwrap().as_str()).unwrap()
                    let (_, [x]) = cap.extract();
                    let a = helper(tree_table, v_table, x);
                    v_table.insert(start, !a);
                }
                7 => {
                    let (_, [x]) = cap.extract();
                    v_table.insert(start, x.parse::<u16>().unwrap());
                }
                _ => unreachable!(),
            }
            break;
        }
    }

    v_table[start]
}

fn day7<'a>(input: &'a [String], start: &'a str) -> HashMap<&'a str, u16> {
    let mut s_table = HashMap::new();
    let mut v_table = HashMap::new();

    // add to tree
    for l in input {
        let (comm, var) = parse_input(l);
        s_table.insert(var, comm);
    }

    //dbg!(&s_table);

    helper(&mut s_table, &mut v_table, &start);

    v_table
}

fn day7_2<'a>(input: &'a [String], start: &'a str, sig: &'a str) -> HashMap<&'a str, u16> {
    let mut s_table = HashMap::new();
    let mut v_table = HashMap::new();

    // add to tree
    for l in input {
        let (comm, var) = parse_input(l);
        s_table.insert(var, comm);
    }

    s_table.insert("b", sig);

    //dbg!(&s_table);

    helper(&mut s_table, &mut v_table, &start);

    v_table
}

fn main() {
    let input = read_file_by_line("../inputs/day7.input");

    let testcase: &str = r#"123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"#;

    //let input: Vec<_> = testcase.lines().map(|l| l.to_string()).collect();
    //dbg!(day7(&input, "i"));

    let part1 = day7(&input, "a")["a"];
    println!("1: {:?}", part1);

    println!("2: {:?}", day7_2(&input, "a", &part1.to_string())["a"])
}

#[cfg(test)]
mod tests {
    use regex::Regex;

    #[test]
    fn test_regex_test() {
        let re = Regex::new(r"^((\d+)|(\w+))$").unwrap();
        dbg!(re.captures("lx"));
    }
}
