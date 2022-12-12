#![feature(int_roundings)]

struct Monkey {
    init_v: Vec<i64>,
    ops: Box<dyn Fn(i64) -> i64>,
    divider: i64,
    check: Box<dyn Fn(i64) -> bool>,
    true_goto: usize,
    false_goto: usize,
}

fn day11(monkeys: &mut [Monkey], times: usize, part2: bool) -> usize {
    let mut xx = multi_round(monkeys, times, part2);
    println!("{:?}", xx);
    xx.sort_by(|a, b| b.partial_cmp(a).unwrap());
    xx.drain(..2).product()
}

fn multi_round(monkeys: &mut [Monkey], times: usize, part2: bool) -> Vec<usize> {
    (0..times)
        .map(|_| one_round(monkeys, part2))
        .reduce(|a, b| {
            let mut x = vec![];
            for i in 0..a.len() {
                x.push(a[i] + b[i]);
            }
            x
        })
        .unwrap()
}

fn one_round(monkeys: &mut [Monkey], part2: bool) -> Vec<usize> {
    let ddd = monkeys.iter().map(|m| m.divider).product();
    (0..monkeys.len())
        .map(|ind| one_move(monkeys, ind, ddd, part2))
        .collect::<Vec<usize>>()
}

fn one_move(monkeys: &mut [Monkey], monkey_idx: usize, m_divider: i64, part2: bool) -> usize {
    for item in monkeys[monkey_idx].init_v.clone() {
        let mut items = (monkeys[monkey_idx].ops)(item);
        if !part2 {
            items = items.div_floor(3)
        }

        let goto = if (monkeys[monkey_idx].check)(items) {
            monkeys[monkey_idx].true_goto
        } else {
            monkeys[monkey_idx].false_goto
        };

        monkeys[goto].init_v.push(items % m_divider)
    }

    let result = monkeys[monkey_idx].init_v.len();
    monkeys[monkey_idx].init_v.clear();

    result
}

fn main() {
    let mut demo_monkeys = vec![
        Monkey {
            init_v: vec![79, 98],
            ops: Box::new(|old| old * 19),
            divider: 23,
            check: Box::new(|items| 0 == items % 23),
            true_goto: 2,
            false_goto: 3,
        },
        Monkey {
            init_v: vec![54, 65, 75, 74],
            ops: Box::new(|old| old + 6),
            divider: 19,
            check: Box::new(|items| 0 == items % 19),
            true_goto: 2,
            false_goto: 0,
        },
        Monkey {
            init_v: vec![79, 60, 97],
            ops: Box::new(|old| old * old),
            divider: 13,
            check: Box::new(|items| 0 == items % 13),
            true_goto: 1,
            false_goto: 3,
        },
        Monkey {
            init_v: vec![74],
            ops: Box::new(|old| old + 3),
            divider: 17,
            check: Box::new(|items| 0 == items % 17),
            true_goto: 0,
            false_goto: 1,
        },
    ];

    let mut monkeys = vec![
        Monkey {
            init_v: vec![91, 66],
            ops: Box::new(|old| old * 13),
            divider: 19,
            check: Box::new(|items| 0 == items % 19),
            true_goto: 6,
            false_goto: 2,
        },
        Monkey {
            init_v: vec![78, 97, 59],
            ops: Box::new(|old| old + 7),
            divider: 5,
            check: Box::new(|items| 0 == items % 5),
            true_goto: 0,
            false_goto: 3,
        },
        Monkey {
            init_v: vec![57, 59, 97, 84, 72, 83, 56, 76],
            ops: Box::new(|old| old + 6),
            divider: 11,
            check: Box::new(|items| 0 == items % 11),
            true_goto: 5,
            false_goto: 7,
        },
        Monkey {
            init_v: vec![81, 78, 70, 58, 84],
            ops: Box::new(|old| old + 5),
            divider: 17,
            check: Box::new(|items| 0 == items % 17),
            true_goto: 6,
            false_goto: 0,
        },
        Monkey {
            init_v: vec![60],
            ops: Box::new(|old| old + 8),
            divider: 7,
            check: Box::new(|items| 0 == items % 7),
            true_goto: 1,
            false_goto: 3,
        },
        Monkey {
            init_v: vec![57, 69, 63, 75, 62, 77, 72],
            ops: Box::new(|old| old * 5),
            divider: 13,
            check: Box::new(|items| 0 == items % 13),
            true_goto: 7,
            false_goto: 4,
        },
        Monkey {
            init_v: vec![73, 66, 86, 79, 98, 87],
            ops: Box::new(|old| old * old),
            divider: 3,
            check: Box::new(|items| 0 == items % 3),
            true_goto: 5,
            false_goto: 2,
        },
        Monkey {
            init_v: vec![95, 89, 63, 67],
            ops: Box::new(|old| old + 2),
            divider: 2,
            check: Box::new(|items| 0 == items % 2),
            true_goto: 1,
            false_goto: 4,
        },
    ];
    //println!("{:?}", day11(&mut demo_monkeys, 20, false));
    println!("{:?}", day11(&mut monkeys, 20, false));
    //println!("{:?}", day11(&mut monkeys, 10000, true));
}
