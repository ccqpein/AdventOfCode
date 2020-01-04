use day11::*;
use std::collections::{HashMap, HashSet};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
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

    fn get_num(&self) -> i32 {
        match self {
            Self::East => 4,
            Self::West => 3,
            Self::South => 2,
            Self::North => 1,
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

#[derive(Clone, Debug)]
struct Coord {
    x: i32,
    y: i32,
    open: HashSet<Direct>,

    havent_been_there: HashSet<Direct>,
}

impl Coord {
    fn new(x: i32, y: i32) -> Self {
        Self {
            x: x,
            y: y,
            open: HashSet::new(),

            havent_been_there: Direct::all(),
        }
    }

    fn set_wall(&mut self, i: i32) {
        let wall = Direct::from(i);

        self.havent_been_there.remove(&wall);
    }

    fn set_open(&mut self, i: i32) {
        let temp = Direct::from(i);

        self.open.insert(temp);
    }

    fn have_been_there(&mut self, i: i32) {
        self.havent_been_there.remove(&Direct::from(i));
    }

    fn part2_all_open(&self) -> HashSet<(i32, i32)> {
        self.open
            .iter()
            .map(|d| {
                (
                    self.x + Direct::get_x_change(&Direct::get_num(d)),
                    self.y + Direct::get_y_change(&Direct::get_num(d)),
                )
            })
            .collect()
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
        self.intcode.clean_input();
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

    fn find_all_path(&mut self, x: &i32, y: &i32) -> HashSet<Direct> {
        let this_coord = self.table.get(&(*x, *y)).unwrap();

        this_coord
            .open
            .intersection(&this_coord.havent_been_there)
            .map(|x| x.clone())
            .collect()
    }

    fn start(&mut self) -> Vec<Direct> {
        let seed: [i32; 4] = [1, 2, 3, 4];
        let mut x = 0;
        let mut y = 0;

        loop {
            //println!("{:?}", (x, y));
            // explore around
            for i in &seed {
                let output = self.run(*i);
                if output == 2 {
                    self.run_stack.push(Direct::from(*i));
                    return self.run_stack.clone();
                };

                match output {
                    0 => {
                        //println!("find wall {:?}", i);
                        //println!("check input  {:?}", self.intcode.input);
                        self.set_wall(&x, &y, i)
                    }
                    1 => {
                        self.set_open(&x, &y, i);
                        self.run(Direct::opposite(i)); // back
                    }
                    _ => panic!(),
                }
            }

            let open = self.find_all_path(&x, &y);
            //println!("{:?}", self.table.get(&(x, y)));
            //println!("{:?}", open);

            if open.is_empty() {
                let last_stack = if let Some(s) = self.run_stack.pop() {
                    s
                } else {
                    return self.run_stack.clone();
                };

                let way_back = Direct::opposite(&Direct::get_num(&last_stack));
                self.run(way_back);
                x += Direct::get_x_change(&way_back);
                y += Direct::get_y_change(&way_back);

                self.table
                    .get_mut(&(x, y))
                    .unwrap()
                    .have_been_there(Direct::opposite(&way_back));
            } else {
                let way_go = Direct::get_num(open.iter().next().unwrap());
                //println!("way to go {}", way_go);
                self.run(way_go);
                x += Direct::get_x_change(&way_go);
                y += Direct::get_y_change(&way_go);

                self.run_stack.push(Direct::from(way_go));

                self.table
                    .entry((x, y))
                    .or_insert(Coord::new(x, y))
                    .have_been_there(Direct::opposite(&way_go));
            }
        }
    }

    fn start_part2(&mut self) -> (i32, i32) {
        let seed: [i32; 4] = [1, 2, 3, 4];
        let mut x = 0;
        let mut y = 0;

        let mut result = (0, 0);

        loop {
            //println!("{:?}", (x, y));
            // explore around
            for i in &seed {
                let output = self.run(*i);
                if output == 2 {
                    //self.run_stack.push(Direct::from(*i));
                    result = (x + Direct::get_x_change(i), y + Direct::get_x_change(i));
                    println!("find oxygen : {:?}", result);
                };

                match output {
                    0 => self.set_wall(&x, &y, i),
                    1 | 2 => {
                        self.set_open(&x, &y, i);
                        self.run(Direct::opposite(i)); // back
                    }
                    _ => panic!(),
                }
            }

            let open = self.find_all_path(&x, &y);
            //println!("{:?}", self.table.get(&(x, y)));
            //println!("{:?}", open);

            if open.is_empty() {
                let last_stack = if let Some(s) = self.run_stack.pop() {
                    s
                } else {
                    return result;
                };

                let way_back = Direct::opposite(&Direct::get_num(&last_stack));
                self.run(way_back);
                x += Direct::get_x_change(&way_back);
                y += Direct::get_y_change(&way_back);

                self.table
                    .get_mut(&(x, y))
                    .unwrap()
                    .have_been_there(Direct::opposite(&way_back));
            } else {
                let way_go = Direct::get_num(open.iter().next().unwrap());
                //println!("way to go {}", way_go);
                self.run(way_go);
                x += Direct::get_x_change(&way_go);
                y += Direct::get_y_change(&way_go);

                self.run_stack.push(Direct::from(way_go));

                self.table
                    .entry((x, y))
                    .or_insert(Coord::new(x, y))
                    .have_been_there(Direct::opposite(&way_go));
            }
        }
    }

    fn print_map(&self, co: (i32, i32)) {
        for r in -21..20 {
            for c in -25..30 {
                if r == co.0 && c == co.1 {
                    print!("O");
                    continue;
                }
                if let Some(co) = self.table.get(&(r as i32, c as i32)) {
                    print!(".")
                } else {
                    print!("#");
                }
            }
            println!("");
        }
    }

    fn oxygen_run(&self, ox: (i32, i32)) -> i32 {
        let mut next_round: HashSet<(i32, i32)> = HashSet::new();
        next_round.insert(ox);

        let mut have_done: HashSet<(i32, i32)> = HashSet::new();
        have_done.insert(ox);

        let mut result = 0;

        while next_round.len() != 0 {
            let mut sset = HashSet::new();
            next_round.iter().for_each(|co| {
                sset = sset
                    .union(&self.table.get(co).unwrap().part2_all_open())
                    .map(|x| x.clone())
                    .collect();
            });

            have_done = have_done.union(&next_round).cloned().collect();
            next_round = sset.difference(&have_done).cloned().collect();

            result += 1;

            println!(
                "minute: {}, have_done: {:?}, next_round: {:?}",
                result, have_done, next_round
            )
        }

        result
    }
}

fn day15(filepath: &str) -> Vec<Direct> {
    //make intcode
    let mut intcode = read_the_damn_intcode(filepath);
    intcode.append(&mut [0; 10000].to_vec()); // give the buffer

    let mut bb = Board::new(&intcode);

    bb.start()
}

fn day15_part2(filepath: &str) {
    //make intcode
    let mut intcode = read_the_damn_intcode(filepath);
    intcode.append(&mut [0; 10000].to_vec()); // give the buffer

    let mut bb = Board::new(&intcode);

    let co = bb.start_part2(); // read all map

    println!("{:?}", bb.oxygen_run(co));
}

fn main() {
    use std::env;
    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();

    //dbg!(day15(&format!("{}{}", path, "/src/day15.input")).len());
    day15_part2(&format!("{}{}", path, "/src/day15.input"));
}
