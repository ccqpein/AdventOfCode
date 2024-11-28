use tools::*;

fn get_coop(num: i32) -> (i32, i32) {
    let mut level = 0;
    let mut last_largest = 0;
    for n in (1..).step_by(2) {
        if num <= n * n {
            last_largest = (n - 2) * (n - 2);
            break;
        } else {
            level += 1;
        }
    }
    //dbg!(level);
    //dbg!(last_largest);

    cal_coop(num - last_largest, level)
}

// offset index start from 0
fn cal_coop(offset: i32, level: i32) -> (i32, i32) {
    let mut ind = (level, -level + 1);
    let rule = vec![(0, 1), (-1, 0), (0, -1), (1, 0)];
    let mut rule_ind = 0;

    for _ in 1..offset {
        ind.0 += rule[rule_ind].0;
        ind.1 += rule[rule_ind].1;

        if ind == (level, level) || ind == (-level, level) || ind == (-level, -level) {
            rule_ind += 1
        }
    }

    ind
}

fn day3(num: i32) -> i32 {
    // (2..=num)
    //     .into_iter()
    //     .map(|n| {
    //         let (a, b) = get_coop(n);
    //         a + b
    //     })
    //     .sum()

    let (a, b) = get_coop(num);
    a + b
}

fn day3_2(num: i32) -> i32 {
    let offset = 100000_i32;
    let mut m: Map<Option<i32>> = Map::new(200000, 200000, None);

    m.set((0 + offset) as usize, (0 + offset) as usize, Some(1))
        .unwrap();

    for n in 2.. {
        let coop = get_coop(n);
        //dbg!(coop);
        let vv = m
            .get_around(((coop.1 + offset) as usize, (coop.0 + offset) as usize))
            .filter_map(|(_, v)| *v)
            .sum::<i32>();
        if vv > num {
            return vv;
        }

        m.set(
            (coop.1 + offset) as usize,
            (coop.0 + offset) as usize,
            Some(vv),
        )
        .unwrap();
    }
    //dbg!(m);

    0
}

fn main() {
    // dbg!(get_coop(23));
    // dbg!(get_coop(15));
    // dbg!(get_coop(14));
    // dbg!(get_coop(2));
    //dbg!(cal_coop(1, 2));

    dbg!(day3(361527));
    //dbg!(day3_2(2));
    dbg!(day3_2(361527));
}
