use tools::*;

fn part1(input: &Vec<String>, day: usize) -> i32 {
    let mut content = input[0]
        .split(",")
        .map(|i| i.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    for ind in 1..=day {
        let num_zero = content.iter().filter(|f| **f == 0).count();
        content = content
            .iter()
            .map(|f| if *f != 0 { f - 1 } else { 6 })
            .collect();
        content.append(&mut vec![8; num_zero]);
    }

    content.iter().count() as i32
}

/// part2 actuall just faster version of part1
fn part2(input: &Vec<String>, big_day: usize) -> usize {
    let mut content = input[0]
        .split(",")
        .map(|i| i.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    let mut record = (0..=8)
        .into_iter()
        .map(|i| content.iter().filter(|f| **f == i).count())
        .collect::<Vec<_>>();

    for d in 1..=big_day {
        let zero = record[0];
        (0..8).into_iter().for_each(|i| record[i] = record[i + 1]);
        record[6] += zero;
        record[8] = zero;
    }

    (0..=8).into_iter().map(|i| record[i]).sum()
}

fn main() {
    let input = read_file_by_line("./src/bin/day6/day6.input");
    //let input = read_file_by_line("./src/bin/day6/day6_demo.input");
    println!("{}", part1(&input, 80));
    //println!("{}", part1(&input, 256));
    println!("{}", part2(&input, 256));
}
