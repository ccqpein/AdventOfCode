use tools::*;

fn part1(input: &Vec<String>) -> usize {
    let le = input[0].len();
    let mut length = vec![(0, 0); le];
    for l in input {
        for c in 0..le {
            match l.get(c..c + 1) {
                Some("0") => length[c].0 += 1,
                Some("1") => length[c].1 += 1,
                _ => panic!(),
            }
        }
    }

    let a = usize::from_str_radix(
        &String::from_iter(length.iter().map(|(z, o)| if z > o { "0" } else { "1" })),
        2,
    )
    .unwrap();
    let b = usize::from_str_radix(
        &String::from_iter(length.iter().map(|(z, o)| if z >= o { "1" } else { "0" })),
        2,
    )
    .unwrap();

    a * b
}

fn part2(input: &Vec<String>) -> usize {
    let length = input[0].len();
    let get_bit_one = |input: &Vec<String>, b| -> (usize, usize) {
        let (mut zero, mut one) = (0, 0);

        for l in input {
            match l.get(b..b + 1) {
                Some("0") => zero += 1,
                Some("1") => one += 1,
                _ => panic!(),
            }
        }

        (zero, one)
    };

    let mut o2 = input.clone();
    for b in 0..length {
        let (zero, one) = get_bit_one(&o2, b);

        let v = if zero > one { "0" } else { "1" };

        o2 = o2
            .into_iter()
            .filter(|l| l.get(b..b + 1).unwrap() == v)
            .collect();

        if o2.len() == 1 {
            break;
        }
    }

    let mut co2 = input.to_owned();
    for b in 0..length {
        let (zero, one) = get_bit_one(&co2, b);

        let v = if zero > one { "1" } else { "0" };

        co2 = co2
            .into_iter()
            .filter(|l| l.get(b..b + 1).unwrap() == v)
            .collect();

        if co2.len() == 1 {
            break;
        }
    }

    usize::from_str_radix(&o2[0], 2).unwrap() * usize::from_str_radix(&co2[0], 2).unwrap()
}

fn main() {
    let input = read_file_by_line("./src/bin/day3/day3.input");
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
