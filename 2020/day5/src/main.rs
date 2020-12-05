use tools::read_file_by_line;

fn find_seat(s: &str) -> (u32, u32) {
    let mut row = (0, 127);
    let mut col = (0, 7);
    for c in s.chars() {
        match c {
            'F' => row = (row.0, row.1 - (row.1 - row.0) / 2 - 1),
            'B' => row = (row.0 + (row.1 - row.0) / 2 + 1, row.1),
            'L' => col = (col.0, col.1 - (col.1 - col.0) / 2 - 1),
            'R' => col = (col.0 + (col.1 - col.0) / 2 + 1, col.1),
            _ => {}
        }
        //println!("row: {:?}, col: {:?}", row, col);
    }

    (row.0, col.0)
}

fn part1(ls: &Vec<String>) -> u32 {
    let mut re = ls
        .iter()
        .map(|s| find_seat(s))
        .map(|(a, b)| a * 8 + b)
        .collect::<Vec<u32>>();

    re.sort_by(|a, b| a.partial_cmp(b).unwrap());

    *re.last().unwrap()
}

fn part2(ls: &Vec<String>, part1_answer: u32) -> u32 {
    use std::collections::HashSet;
    let mut re = ls
        .iter()
        .map(|s| find_seat(s))
        .map(|(a, b)| a * 8 + b)
        .collect::<HashSet<u32>>();

    for a in 0..part1_answer {
        if re.get(&a).is_none() && re.get(&(a + 1)).is_some() && re.get(&(a - 1)).is_some() {
            return a;
        }
    }
    0
}

fn main() {
    let input = read_file_by_line(String::from("./src/day5.input"));
    println!("{:?}", part1(&input));

    println!("{:?}", part2(&input, part1(&input)));
}
