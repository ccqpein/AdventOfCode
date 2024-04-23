use std::collections::HashMap;

use itertools::*;
use regex::Regex;
use tools::*;

fn day12(input: &[String], time: usize) {
    let [init, rules] = input.split(|s| *s == String::new()).collect::<Vec<_>>()[..] else {
        unreachable!()
    };

    //dbg!(&init);
    let mut init = init[0]
        .split(": ")
        .nth(1)
        .unwrap()
        .clone()
        .chars()
        .enumerate()
        .map(|(ind, e)| (ind as i32, e))
        .collect::<Vec<_>>();

    //dbg!(init);

    init = [
        (init[0].0 - 5..init[0].0)
            .map(|ind| (ind, '.'))
            .collect::<Vec<_>>(),
        init.clone(),
        (init.last().unwrap().0 + 1..init.last().unwrap().0 + 6)
            .map(|ind| (ind, '.'))
            .collect::<Vec<_>>(),
    ]
    .concat();
    dbg!(&init);

    let re = Regex::new(r#"^(.*) => (.*)$"#).unwrap();
    let rule_table = rules
        .iter()
        .map(|rule| {
            let (_, [x, y]) = re.captures(rule).unwrap().extract();
            (x.chars().collect(), y.chars().next().unwrap())
        })
        .collect::<HashMap<Vec<_>, char>>();

    dbg!(&rule_table);

    //
    for t in 1..=time {
        init = init
            .into_iter()
            .tuple_windows()
            .into_iter()
            .map(|(a, b, c, d, e)| {
                //dbg!([a, b, c, d, e]);
                if let Some(x) = rule_table.get(&vec![a.1, b.1, c.1, d.1, e.1]) {
                    //dbg!(x);
                    (c.0, *x)
                } else {
                    (c.0, '.')
                }
            })
            .collect();

        init = [
            (init[0].0 - 5..init[0].0)
                .map(|ind| (ind, '.'))
                .collect::<Vec<_>>(),
            init.clone(),
            (init.last().unwrap().0 + 1..init.last().unwrap().0 + 6)
                .map(|ind| (ind, '.'))
                .collect::<Vec<_>>(),
        ]
        .concat();

        let vv = init
            .iter()
            .filter(|(_, e)| *e == '#')
            .map(|(n, _)| n)
            .sum::<i32>();

        println!("time: {}, value is: {}", t, vv);
    }

    //dbg!(&init);
    //dbg!(String::from_iter(init.iter().map(|(_, c)| c)));

    dbg!(init
        .iter()
        .filter(|(_, e)| *e == '#')
        .map(|(n, _)| n)
        .sum::<i32>());
}

fn main() {
    let input = read_file_by_line("../inputs/day12.input");
    //let input = read_file_by_line("../inputs/day12_demo.input");

    day12(&input, 20);
    //day12(&input, 1000);

    println!("part2: {}", (50000000000_usize - 170) * 8 + 1317)
}
