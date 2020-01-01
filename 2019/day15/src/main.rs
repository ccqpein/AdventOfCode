use day11::*;
use std::collections::{HashMap, HashSet};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Direct {
    East,  // 4
    West,  // 3
    South, // 2
    North, // 1
}

impl Direct {
    fn all() -> HashSet<Self> {
        vec![Self::East, Self::West, Self::South, Self::North]
            .iter()
            .cloned()
            .collect()
    }

    fn get_x_change(a: &i32) -> i32 {
        match *a {
            1 => -1,
            2 => 1,
            3 => 0,
            4 => 0,
            _ => panic!(),
        }
    }

    fn get_y_change(a: &i32) -> i32 {
        match *a {
            1 => 0,
            2 => 0,
            3 => -1,
            4 => 1,
            _ => panic!(),
        }
    }

    fn opposite(a: &i32) -> i32 {
        match *a {
            1 => 2,
            2 => 1,
            3 => 4,
            4 => 3,
            _ => panic!(),
        }
    }
}
impl From<i32> for Direct {
    fn from(a: i32) -> Self {
        match a {
            1 => Direct::North,
            2 => Direct::South,
            3 => Direct::West,
            4 => Direct::East,
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
struct Coord {
    x: i32,
    y: i32,
    open: HashSet<Direct>,
    unknown: HashSet<Direct>,
}

impl Coord {
    fn new(x: i32, y: i32) -> Self {
        Self {
            x: x,
            y: y,
            open: HashSet::new(),
            unknown: Direct::all(),
        }
    }

    fn set_wall(&mut self, i: i32) {
        let wall = Direct::from(i);

        self.unknown.remove(&wall);
    }

    fn set_open(&mut self, i: i32) {
        let wall = Direct::from(i);

        self.unknown.remove(&wall);
        self.open.insert(wall);
    }
}

struct Board {
    intcode: Intcode,
    table: HashMap<(i32, i32), Coord>,

    run_stack: Vec<Direct>,
}

impl Board {
    fn new(intcode: &Vec<i64>) -> Self {
        Self {
            intcode: Intcode::new(intcode),
            table: HashMap::new(),
            run_stack: vec![],
        }
    }

    fn run(&mut self, input: i32) -> i64 {
        self.intcode.run(Some(input as i64));
        self.intcode.output.pop().unwrap()
    }

    fn set_wall(&mut self, x: &i32, y: &i32, i: &i32) {
        self.table
            .entry((*x, *y))
            .or_insert(Coord::new(*x, *y))
            .set_wall(*i)
    }

    fn set_open(&mut self, x: &i32, y: &i32, i: &i32) {
        self.table
            .entry((*x, *y))
            .or_insert(Coord::new(*x, *y))
            .set_open(*i)
    }

    fn start(&mut self) -> Vec<Direct> {
        let seed: [i32; 4] = [1, 2, 3, 4];
        let mut x = 0;
        let mut y = 0;
        //let mut current_co = Coord::new(0, 0);
        loop {
            for i in &seed {
                let output = self.run(*i);
                if output == 2 {
                    self.run_stack.push(Direct::from(*i));
                    return self.run_stack.clone();
                };

                match output {
                    0 => self.set_wall(&x, &y, i),
                    1 => {
                        self.set_open(&x, &y, i);
                        self.run(Direct::opposite(i)); // back
                    }
                    _ => panic!(),
                }

                //x += Direct::get_x_change(i);
                //y += Direct::get_y_change(i);
                //self.run_stack.push(Direct::from(*i));
            }
        }
    }
}

fn day15(filepath: &str) -> Vec<Direct> {
    //make intcode
    let mut intcode = read_the_damn_intcode(filepath);
    intcode.append(&mut [0; 10000].to_vec()); // give the buffer

    let mut bb = Board::new(&intcode);

    bb.run_stack
}

fn main() {}
