use itertools::min;
use regex::*;
use tools::*;

fn day2(input: &[String]) -> i32 {
    let re = Regex::new(r"^(\d+)x(\d+)x(\d+)$").unwrap();
    let mut sum = 0;
    for line in input {
        for (_, [l, w, h]) in re.captures_iter(line).map(|c| c.extract()) {
            //println!("{}, {}, {}", l, w, h);
            sum += 2 * l.parse::<i32>().unwrap() * w.parse::<i32>().unwrap()
                + 2 * w.parse::<i32>().unwrap() * h.parse::<i32>().unwrap()
                + 2 * h.parse::<i32>().unwrap() * l.parse::<i32>().unwrap();
            sum += min([
                l.parse::<i32>().unwrap() * w.parse::<i32>().unwrap(),
                w.parse::<i32>().unwrap() * h.parse::<i32>().unwrap(),
                h.parse::<i32>().unwrap() * l.parse::<i32>().unwrap(),
            ]
            .into_iter())
            .unwrap()
        }
    }

    sum
}

fn day2_part2(input: &[String]) -> i32 {
    let re = Regex::new(r"^(\d+)x(\d+)x(\d+)$").unwrap();
    let mut sum = 0;
    for line in input {
        for (_, [l, w, h]) in re.captures_iter(line).map(|c| c.extract()) {
            //println!("{}, {}, {}", l, w, h);
            let mut x = vec![
                l.parse::<i32>().unwrap(),
                w.parse::<i32>().unwrap(),
                h.parse::<i32>().unwrap(),
            ];
            x.sort();

            match &x[..] {
                [a, b, c, ..] => sum += 2 * a + 2 * b + a * b * c,
                _ => unreachable!(),
            }
        }
    }

    sum
}

fn main() {
    let input = read_file_by_line("../inputs/day2.input");
    println!("{:?}", day2(&input));
    println!("{:?}", day2_part2(&input));
}
