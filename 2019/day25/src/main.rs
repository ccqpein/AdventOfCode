use day11::*;

fn parse_command(s: String) -> Vec<i64> {
    let mut r = s.chars().map(|x| x as i64).collect::<Vec<i64>>();
    r
}

fn main() {
    use std::env;
    use std::io::{stdin, stdout};

    // read intcode
    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();

    //make intcode
    let mut intcode = read_the_damn_intcode(&format!("{}/src/{}", path, "day25.input"));
    intcode.append(&mut [0; 10000].to_vec()); // give the buffer

    let mut ic = Intcode::new(&intcode);

    let mut buffer = String::new();
    loop {
        stdin().read_line(&mut buffer).unwrap();
        println!("{}", buffer);
    }
}
