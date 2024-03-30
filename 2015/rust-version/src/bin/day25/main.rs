use std::collections::HashMap;

fn test_input(end: usize) {
    let mut start = 20151125_usize;
    let mut cache = HashMap::new();
    cache.insert(start, vec![1]);
    for i in 2..=end {
        start = (start * 252533) % 33554393;
        let en = cache.entry(start).or_insert(vec![]);
        if en.len() != 0 {
            println!(
                "value {} on step {} has last visit {}",
                start,
                i,
                en.last().unwrap()
            )
        }
        en.push(i);
    }
}

fn cal_step(row: usize, col: usize) -> usize {
    let mut r = 1;
    for rr in 1..row {
        r += rr
    }

    //dbg!(r);
    let mut c = r;
    for cc in 1..col {
        c += row + cc
    }
    //dbg!(c);
    c
}

fn day25(r: usize, c: usize) -> usize {
    let end = cal_step(r, c);
    let mut start = 20151125_usize;
    for i in 2..=end {
        start = (start * 252533) % 33554393;
    }

    start
}

fn main() {
    // row 3010, column 3019.
    //test_input();
    assert_eq!(18, cal_step(4, 3));
    assert_eq!(20, cal_step(2, 5));
    println!("1: {}", day25(3010, 3019));
}
