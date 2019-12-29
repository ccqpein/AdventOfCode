use day11::*;
use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Coordinate {
    x: i64,
    y: i64,
}

impl Coordinate {
    fn is_origin(&self) -> bool {
        if self.x == 0 && self.y == 0 {
            return true;
        }
        false
    }

    fn new() -> Self {
        Coordinate { x: 0, y: 0 }
    }
}

fn day13(intcode_output: &Vec<i64>) -> i64 {
    let mut input = intcode_output.clone();
    let mut table: HashMap<Coordinate, i64> = HashMap::new();

    while input.len() > 0 {
        let this_step: Vec<i64> = input.drain(..3).collect();
        table.insert(
            Coordinate {
                x: this_step[0],
                y: this_step[1],
            },
            this_step[2],
        );
    }

    table.values().filter(|x| **x == 2).count() as i64
}

fn day13_part2(intcode: &mut Intcode) -> i64 {
    let mut table: HashMap<Coordinate, i64> = HashMap::new();
    loop {
        let mut input = intcode.output.clone();
        table = HashMap::new();

        while input.len() > 0 {
            let this_step: Vec<i64> = input.drain(..3).collect();
            table.insert(
                Coordinate {
                    x: this_step[0],
                    y: this_step[1],
                },
                this_step[2],
            );
        }

        if intcode.status == "Terminal" {
            break;
        }

        // because iter hashtable does not follow same order all the time
        // maybe need run several times to find answer
        let mut ball = Coordinate::new();
        for (k, v) in &table {
            if *v == 4 {
                ball = *k;
                break;
            }
        }
        let mut padd = Coordinate::new();
        for (k, v) in &table {
            if *v == 3 {
                padd = *k;
                break;
            }
        }

        let new_input = if ball.x < padd.x {
            -1
        } else if ball.x > padd.x {
            1
        } else {
            0
        };

        //println!("input {}", new_input);
        intcode.output = vec![];
        intcode.run(Some(new_input));
    }

    *table.get(&Coordinate { x: -1, y: 0 }).unwrap()
}

fn main() {
    use std::env;
    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();
    let mut intcode = read_the_damn_intcode(&format!("{}{}", path, "/src/day13.input"));
    intcode.append(&mut [0; 10000].to_vec());

    // let mut a = Intcode::new(&intcode);
    // a.run(0);
    // println!("{:?}", day13(&a.output));

    intcode[0] = 2;
    let mut a = Intcode::new(&intcode);
    a.run(None);

    //:= MARK: if answer is 0, run several times.
    println!("{:?}", day13_part2(&mut a));
}
