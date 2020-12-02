use tools::read_file_by_line;

fn check_if_valid(ss: &str) -> bool {
    let mut ls = ss.split_whitespace();
    let mut times = ls.next().unwrap().split('-');

    // times
    let (min, max) = (
        times.next().unwrap().parse::<usize>().unwrap(),
        times.next().unwrap().parse::<usize>().unwrap(),
    );

    // char
    let cha = ls.next().unwrap().chars().next().unwrap();

    let password = ls.next().unwrap().chars();

    let count = password.filter(|c| *c == cha).count();
    count <= max && count >= min
}

fn check_if_valid_part2(ss: &str) -> bool {
    let mut ls = ss.split_whitespace();
    let mut times = ls.next().unwrap().split('-');

    // times
    let (first, second) = (
        times.next().unwrap().parse::<usize>().unwrap(),
        times.next().unwrap().parse::<usize>().unwrap(),
    );

    // char
    let cha = &ls.next().unwrap()[0..1];

    let password = ls.next().unwrap();

    (password[first - 1..first] == *cha) ^ (password[second - 1..second] == *cha)
}

fn main() {
    let input = read_file_by_line(String::from("./src/day2.input"));
    println!(
        "first answer: {}",
        input.iter().filter(|ss| check_if_valid(ss)).count()
    );

    //dbg!(check_if_valid_part2("1-2 a: ba"));
    println!(
        "second answer: {}",
        input.iter().filter(|ss| check_if_valid_part2(ss)).count()
    );
}
