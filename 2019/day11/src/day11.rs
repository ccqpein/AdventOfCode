use day11::*;
use std::collections::HashMap;

#[derive(Hash)]
enum Direction {
    UP,
    DOWN,
    RIGHT,
    LEFT,
}

#[derive(Hash, Debug)]
struct Panel {
    row: i32,
    col: i32,
}

impl PartialEq for Panel {
    fn eq(&self, other: &Self) -> bool {
        if self.row == other.row && self.col == other.col {
            true
        } else {
            false
        }
    }
}

impl Eq for Panel {}

impl Panel {
    fn new() -> Self {
        Panel { row: 0, col: 0 }
    }

    fn one_move(&mut self, code: (i32, i32), direction: &Direction) -> (Self, Direction) {
        match direction {
            Direction::UP => {
                if code.1 == 0 {
                    (
                        Self {
                            row: self.row,
                            col: self.col - 1,
                        },
                        Direction::LEFT,
                    )
                } else {
                    (
                        Self {
                            row: self.row,
                            col: self.col + 1,
                        },
                        Direction::RIGHT,
                    )
                }
            }
            Direction::DOWN => {
                if code.1 == 0 {
                    (
                        Self {
                            row: self.row,
                            col: self.col + 1,
                        },
                        Direction::RIGHT,
                    )
                } else {
                    (
                        Self {
                            row: self.row,
                            col: self.col - 1,
                        },
                        Direction::LEFT,
                    )
                }
            }
            Direction::LEFT => {
                if code.1 == 0 {
                    (
                        Self {
                            row: self.row - 1,
                            col: self.col,
                        },
                        Direction::DOWN,
                    )
                } else {
                    (
                        Self {
                            row: self.row + 1,
                            col: self.col,
                        },
                        Direction::UP,
                    )
                }
            }
            Direction::RIGHT => {
                if code.1 == 0 {
                    (
                        Self {
                            row: self.row + 1,
                            col: self.col,
                        },
                        Direction::UP,
                    )
                } else {
                    (
                        Self {
                            row: self.row - 1,
                            col: self.col,
                        },
                        Direction::DOWN,
                    )
                }
            }
        }
    }
}

fn run(intcode: Vec<i64>, first_color: i64) -> HashMap<Panel, i8> {
    let mut table: HashMap<Panel, i8> = HashMap::new();
    let mut cusor = Panel::new();
    let mut direction = Direction::UP;

    let mut a = Intcode::new(&intcode);
    a.run(Some(first_color));
    let mut steps = a.output;
    a.output = vec![];

    loop {
        let (cusor_new, direction_new) =
            cusor.one_move((steps[0] as i32, steps[1] as i32), &direction);
        *table.entry(cusor).or_insert(0) = steps[0] as i8;

        cusor = cusor_new;
        direction = direction_new;

        let new_input = match table.get(&cusor) {
            Some(v) => *v,
            None => 0,
        };

        a.run(Some(new_input as i64));
        if a.status == "Terminal".to_string() {
            break;
        }

        steps = a.output;
        a.output = vec![];
    }

    //println!("{:?}", table.keys().len());
    table
}

fn day11_part2(intcode: Vec<i64>) {
    let t = run(intcode, 1);
    //println!("{:?}", t);
    for r in -5..5 {
        let mut line = vec![];
        for c in 0..41 {
            line.push(if let Some(v) = t.get(&Panel { row: r, col: c }) {
                if *v == 1 {
                    b'*'
                } else {
                    b' '
                }
            } else {
                b'_'
            })
        }
        println!("{:?}", String::from_utf8(line).unwrap());
    }
}

fn main() {
    use std::env;
    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();
    let mut intcode = read_the_damn_intcode(&format!("{}{}", path, "/src/day11.input"));
    intcode.append(&mut [0; 10000].to_vec());
    //run(intcode, 0);
    day11_part2(intcode);
}
