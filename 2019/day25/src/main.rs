use day11::*;
use std::str;

fn parse_command(s: &String) -> Vec<i64> {
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

    loop {
        let mut buffer = String::new();
        stdin().read_line(&mut buffer).unwrap();

        ic.input = parse_command(&buffer);
        ic.run(None);
        let output = String::from_utf8(
            ic.output
                .clone()
                .iter()
                .map(|x| *x as u8)
                .collect::<Vec<u8>>(),
        )
        .unwrap();

        if ic.status == "Terminal" {
            for l in output.lines() {
                if l != "" {
                    println!("{:?}", l)
                };
            }
            break;
        }

        for l in output.lines() {
            if l != "" {
                println!("{:?}", l)
            };
        }
        ic.output = vec![];
    }
}
