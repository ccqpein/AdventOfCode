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
    content.sort();

    let mut record = [0; 9];
    record[0] = content.iter().filter(|f| **f == 0).count();
    record[1] = content.iter().filter(|f| **f == 1).count();
    record[2] = content.iter().filter(|f| **f == 2).count();
    record[3] = content.iter().filter(|f| **f == 3).count();
    record[4] = content.iter().filter(|f| **f == 4).count();
    record[5] = content.iter().filter(|f| **f == 5).count();
    record[6] = content.iter().filter(|f| **f == 6).count();
    record[7] = content.iter().filter(|f| **f == 7).count();
    record[8] = content.iter().filter(|f| **f == 8).count();

    for d in 1..=big_day {
        let zero = record[0];
        record[0] = record[1];
        record[1] = record[2];
        record[2] = record[3];
        record[3] = record[4];
        record[4] = record[5];
        record[5] = record[6];
        record[6] = record[7] + zero;
        record[7] = record[8];
        record[8] = zero;
        // content = content
        //     .iter()
        //     .map(|f| if *f != 0 { f - 1 } else { 6 })
        //     .collect();
        // content.append(&mut vec![8; num_zero]);
        //println!("day {}: {:?}", d, record);
        //let this_day_count = content.iter().count() as i32;
    }

    (0..=8).into_iter().map(|i| record[i]).sum()
}

fn main() {
    let input = read_file_by_line("./src/bin/day6/day6.input");
    //let input = read_file_by_line("./src/bin/day6/day6_demo.input");
    //println!("{}", part1(&input, 18));
    //println!("{}", part1(&input, 256));
    println!("{}", part2(&input, 256));
}
