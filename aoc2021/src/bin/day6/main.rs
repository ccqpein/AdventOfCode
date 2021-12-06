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

fn part2(input: &Vec<String>, big_day: usize) -> usize {
    let mut content = input[0]
        .split(",")
        .map(|i| i.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    let mut record: Vec<i32> = vec![];
    for d in 1..=big_day {
        let num_zero = content.iter().filter(|f| **f == 0).count();
        content = content
            .iter()
            .map(|f| if *f != 0 { f - 1 } else { 6 })
            .collect();
        content.append(&mut vec![8; num_zero]);

        let this_day_count = content.iter().count() as i32;
        // if let Some(a) = record
        //     .iter()
        //     .enumerate()
        //     .filter(|&(d, c)| *c == this_day_count)
        //     .next()
        // {
        //     println!("day {} fish number is {} as same as day {}", a.0, a.1, d);
        // }

        record.push(this_day_count);
    }

    println!("{:?}", record);
    let aa = (1..record.len())
        .into_iter()
        .map(|ind| record[ind] - record[ind - 1])
        .collect::<Vec<i32>>();
    println!("{:?}", aa);
    0
}

fn main() {
    let input = read_file_by_line("./src/bin/day6/day6.input");
    //let input = read_file_by_line("./src/bin/day6/day6_demo.input");
    //println!("{}", part1(&input, 18));
    //println!("{}", part1(&input, 256));
    println!("{}", part2(&input, 128));
}
