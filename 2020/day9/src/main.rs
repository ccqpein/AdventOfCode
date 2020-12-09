use tools::read_file_by_line;

fn parser(ls: &Vec<String>) -> Vec<u64> {
    ls.iter().map(|s| s.parse::<u64>().unwrap()).collect()
}

fn sort_between(a: &Vec<u64>, i: usize, j: usize, buffer: &mut Vec<u64>) {
    buffer.clear();
    buffer.extend_from_slice(&a[i..j]);
    buffer.sort();
}

fn find_sum(buffer: &Vec<u64>, a: u64) -> bool {
    for j in (0..buffer.len()).rev() {
        for i in 0..j {
            if buffer[j] + buffer[i] > a {
                break;
            }
            if buffer[j] + buffer[i] == a {
                return true;
            }
        }
    }
    false
}

fn part1(input: &Vec<String>) -> u64 {
    let a = parser(input);
    let mut buffer = vec![];
    for ind in 25..a.len() {
        sort_between(&a, ind - 25, ind, &mut buffer);
        if !find_sum(&buffer, a[ind]) {
            return a[ind];
        }
    }
    0
}

fn part2(input: &Vec<String>) -> (u64, u64) {
    let a = parser(input);
    let mut buffer = vec![];
    let x = part1(input);

    for le in 2..a.len() {
        for start in 0..a.len() - le {
            sort_between(&a, start, start + le, &mut buffer);
            if buffer.iter().sum::<u64>() == x {
                return (buffer[0], buffer[le - 1]);
            }
        }
    }
    (0, 0)
}

fn main() {
    let input = read_file_by_line(String::from("./src/day9.input"));
    println!("{:?}", part1(&input));
    let r2 = part2(&input);
    println!("{:?} -> {:?}", r2, r2.0 + r2.1);
}
