use tools::*;

fn parse_input(input: &Vec<String>) -> impl Iterator<Item = i32> + '_ {
    input
        .iter()
        .map(|line| {
            let mut a = line.split(' ').into_iter();
            match (a.next(), a.next()) {
                (Some(_), None) => vec![0],
                (Some(_), Some(num)) => vec![0, num.parse::<i32>().unwrap()],
                _ => unreachable!(),
            }
        })
        .flatten()
}

fn day10(input: &Vec<String>) -> i32 {
    let parsed_input = parse_input(input);
    let mut flag = 20;
    let mut count = 1;
    let mut result = 0;

    for (i, n) in parsed_input.enumerate() {
        if 1 + i as i32 == flag {
            result += count * flag;
            flag += 40;
        }
        count += n;
    }
    result
}

fn day10_part2(input: &Vec<String>) {
    let parsed_input = parse_input(input);

    let mut sprite_mid = 1;
    let mut result = vec![];
    for (i, n) in parsed_input.enumerate() {
        if (i as i32 % 40 - sprite_mid).abs() <= 1 {
            result.push('#');
        } else {
            result.push('.');
        }

        sprite_mid += n;
    }

    result
        .chunks(40)
        .map(|line| String::from_iter(line))
        .for_each(|line| println!("{}", line));
}

fn main() {
    //let input = read_file_by_line("./inputs/day10_demo.input");
    let input = read_file_by_line("./inputs/day10.input");
    println!("{}", day10(&input));
    day10_part2(&input);
}
