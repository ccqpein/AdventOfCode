use itertools::*;
use tools::*;

fn adj(ll: &[i32]) -> bool {
    let flag = ll[1] - ll[0] < 0;
    for (a, b) in ll.iter().tuple_windows() {
        if flag {
            if !((a - b) <= 3 && (a - b) >= 1) {
                return false;
            }
        } else {
            if !((b - a) <= 3 && (b - a) >= 1) {
                return false;
            }
        }
    }

    true
}

fn adj2(ll: &[i32]) -> bool {
    for ind in 0..ll.len() {
        let mut a = ll.get(0..ind).unwrap().to_vec();
        let mut b = ll.get(ind + 1..).unwrap().to_vec();
        a.append(&mut b);
        if adj(&a) {
            return true;
        }
    }
    false
}

fn day2(input: &[String], part2: bool) -> i32 {
    let input = input.into_iter().map(|l| {
        l.split(' ')
            .map(|d| d.parse::<i32>().unwrap())
            .collect::<Vec<_>>()
    });
    let mut count = 0;

    for (_, l) in input.into_iter().enumerate() {
        let mut ll2 = l.clone();
        let mut ll3 = l.clone();

        ll2.sort_by(|a, b| a.cmp(b));
        ll3.sort_by(|a, b| b.cmp(a));

        if ((l == ll2 || l == ll3) && adj(&l)) || (part2 && adj2(&l)) {
            //println!("{ind}: {:?}", l);
            count += 1
        }
    }
    count
}

fn main() {
    let input = read_file_by_line("../inputs/day2.input");
    //let input = read_file_by_line("../inputs/day2_demo.input");
    dbg!(day2(&input, false));
    dbg!(day2(&input, true));
}
