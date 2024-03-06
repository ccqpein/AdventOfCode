use std::collections::HashMap;

use regex::Regex;
use tools::*;

fn parse_input(l: &str) -> (&str, &str) {
    let re = Regex::new(r"^(.+) -> (.+)").unwrap();
    let (_, [comm, target]) = re.captures(l).unwrap().extract();
    (comm, target)
}

fn day7(input: &[String]) -> HashMap<&str, u16> {
    let mut s_table = HashMap::new();

    let rex = [
        (1, Regex::new(r"^((\d+)|(\w+))$").unwrap()),
        (2, Regex::new(r"^(.+) AND (.+)$").unwrap()),
        (3, Regex::new(r"^(.+) OR (.+)$").unwrap()),
        (4, Regex::new(r"^(.+) LSHIFT (\d+)$").unwrap()),
        (5, Regex::new(r"^(.+) RSHIFT (\d+)$").unwrap()),
        (6, Regex::new(r"^NOT (.+)$").unwrap()),
    ];

    for l in input {
        let (comm, var) = parse_input(l);
        //dbg!(&comm);
        //dbg!(&var);
        for (i, r) in rex.iter() {
            if let Some(cap) = r.captures(comm) {
                //dbg!(&cap);
                match i {
                    1 => {
                        *s_table.entry(var).or_insert(0) =
                            cap.get(0).unwrap().as_str().parse::<u16>().unwrap_or(0);
                    }
                    2 => {
                        *s_table.entry(var).or_insert(0) = {
                            let (_, [x, y]) = cap.extract();
                            s_table.entry(x);
                            s_table.entry(y);

                            s_table[x] & s_table[y]
                        }
                    }
                    3 => {
                        *s_table.entry(var).or_insert(0) = {
                            let (_, [x, y]) = cap.extract();
                            s_table.entry(x);
                            s_table.entry(y);
                            s_table[x] | s_table[y]
                        }
                    }
                    4 => {
                        *s_table.entry(var).or_insert(0) = {
                            let (_, [x, y]) = cap.extract();
                            s_table.entry(x);
                            //s_table.entry(y).or_insert(0);
                            s_table[x] << y.parse::<u16>().unwrap()
                        }
                    }
                    5 => {
                        *s_table.entry(var).or_insert(0) = {
                            let (_, [x, y]) = cap.extract();
                            s_table.entry(x);
                            //s_table.entry(y).or_insert(0);
                            s_table[x] >> y.parse::<u16>().unwrap()
                        }
                    }
                    6 => {
                        *s_table.entry(var).or_insert(0) = {
                            let (_, [x]) = cap.extract();
                            s_table.entry(x);
                            !s_table[x]
                        }
                    }
                    _ => unreachable!(),
                }
                break;
            }
        }
    }

    s_table
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

    let input: Vec<_> = testcase.lines().map(|l| l.to_string()).collect();
    dbg!(day7(&input));

    //println!("1: {:?}", day7(&input).get("a"))
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
