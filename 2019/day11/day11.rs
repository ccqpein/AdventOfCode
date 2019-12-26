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

#[derive(Debug)]
struct Boost {
    intcode: Vec<i64>,
    output: Vec<i64>,
    relative_base: usize,
    index: usize,
    input: Vec<i64>,
    status: String,
}

impl Boost {
    fn new(intcode: &Vec<i64>) -> Self {
        Boost {
            intcode: intcode.clone(),
            output: vec![],
            relative_base: 0,
            index: 0,
            input: vec![],
            status: String::new(),
        }
    }

    fn run(&mut self, input: i64) {
        self.input.push(input);
        loop {
            if self.index >= self.intcode.len() {
                return;
            }

            match self.intcode[self.index] {
                1 => {
                    let (a, b, c) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                        self.intcode[self.index + 3] as usize,
                    );

                    self.intcode[c] = self.intcode[a] + self.intcode[b];
                    self.index += 4
                }

                2 => {
                    let (a, b, c) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                        self.intcode[self.index + 3] as usize,
                    );

                    self.intcode[c] = self.intcode[a] * self.intcode[b];
                    self.index += 4
                }

                3 => {
                    if self.input.len() == 0 {
                        break;
                    }

                    let a = self.intcode[self.index + 1] as usize;

                    self.intcode[a] = self.input[0];
                    self.input.drain(..1);
                    self.index += 2
                }
                4 => {
                    let a = self.intcode[self.index + 1] as usize;

                    self.output.push(self.intcode[a]);
                    self.index += 2
                }

                5 => {
                    let (a, b) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                    );

                    if self.intcode[a] != 0 {
                        self.index = self.intcode[b] as usize;
                    } else {
                        self.index += 3;
                    }
                }

                6 => {
                    let (a, b) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                    );

                    if self.intcode[a] == 0 {
                        self.index = self.intcode[b] as usize;
                    } else {
                        self.index += 3;
                    }
                }

                7 => {
                    let (a, b, c) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                        self.intcode[self.index + 3] as usize,
                    );

                    if self.intcode[a] < self.intcode[b] {
                        self.intcode[c] = 1;
                    } else {
                        self.intcode[c] = 0;
                    }

                    self.index += 4;
                }

                8 => {
                    let (a, b, c) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                        self.intcode[self.index + 3] as usize,
                    );

                    if self.intcode[a] == self.intcode[b] {
                        self.intcode[c] = 1;
                    } else {
                        self.intcode[c] = 0;
                    }

                    self.index += 4;
                }

                9 => {
                    self.relative_base +=
                        self.intcode[self.intcode[self.index + 1] as usize] as usize;
                    self.index += 2;
                }

                99 => {
                    self.status = "Terminal".to_string();
                    return;
                }

                opcode @ _ => {
                    let (a, b, c) = (
                        self.intcode[self.index + 1],
                        self.intcode[self.index + 2],
                        self.intcode[self.index + 3],
                    );

                    // code = DE, C, B, A
                    let code = vec![
                        opcode % 100,
                        (opcode / 100) % 10,
                        (opcode / 1000) % 10,
                        opcode / 10000,
                    ];
                    let (aa, bb, cc);

                    match code[0] {
                        1 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b as usize] as i64
                            } else if code[2] == 2 {
                                bb = self.intcode[(b + self.relative_base as i64) as usize] as i64
                            } else {
                                bb = b as i64
                            }

                            if code[3] == 2 {
                                cc = c + self.relative_base as i64
                            } else {
                                cc = c as i64
                            }

                            self.intcode[cc as usize] = aa + bb;
                            self.index += 4;
                        }

                        2 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b as usize] as i64
                            } else if code[2] == 2 {
                                bb = self.intcode[(b + self.relative_base as i64) as usize] as i64
                            } else {
                                bb = b as i64
                            }

                            if code[3] == 2 {
                                cc = c + self.relative_base as i64
                            } else {
                                cc = c as i64
                            }

                            self.intcode[cc as usize] = aa * bb;

                            self.index += 4;
                        }

                        3 => {
                            if code[1] == 0 {
                                self.intcode[a as usize] = input
                            } else if code[1] == 2 {
                                self.intcode[(a + self.relative_base as i64) as usize] = input
                            } else {
                                panic!();
                            }

                            self.index += 2;
                        }

                        4 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            self.output.push(aa);
                            self.index += 2;
                        }

                        5 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b as usize] as i64
                            } else if code[2] == 2 {
                                bb = self.intcode[(b + self.relative_base as i64) as usize] as i64
                            } else {
                                bb = b as i64
                            }

                            if aa != 0 {
                                self.index = bb as usize;
                            } else {
                                self.index += 3
                            }
                        }

                        6 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b as usize] as i64
                            } else if code[2] == 2 {
                                bb = self.intcode[(b + self.relative_base as i64) as usize] as i64
                            } else {
                                bb = b as i64
                            }

                            if aa == 0 {
                                self.index = bb as usize;
                            } else {
                                self.index += 3
                            }
                        }

                        7 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b as usize] as i64
                            } else if code[2] == 2 {
                                bb = self.intcode[(b + self.relative_base as i64) as usize] as i64
                            } else {
                                bb = b as i64
                            }

                            if code[3] == 2 {
                                cc = c + self.relative_base as i64
                            } else {
                                cc = c as i64
                            }

                            if aa < bb {
                                self.intcode[cc as usize] = 1;
                            } else {
                                self.intcode[cc as usize] = 0;
                            }

                            self.index += 4;
                        }
                        8 => {
                            if code[1] == 0 {
                                aa = self.intcode[a as usize] as i64
                            } else if code[1] == 2 {
                                aa = self.intcode[(a + self.relative_base as i64) as usize] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b as usize] as i64
                            } else if code[2] == 2 {
                                bb = self.intcode[(b + self.relative_base as i64) as usize] as i64
                            } else {
                                bb = b as i64
                            }

                            if code[3] == 2 {
                                cc = c + (self.relative_base as i64)
                            } else {
                                cc = c as i64
                            }

                            if aa == bb {
                                self.intcode[cc as usize] = 1;
                            } else {
                                self.intcode[cc as usize] = 0;
                            }

                            self.index += 4;
                        }

                        9 => {
                            if code[1] == 1 {
                                self.relative_base = (a + self.relative_base as i64) as usize;
                            } else if code[1] == 0 {
                                self.relative_base += self.intcode[a as usize] as usize;
                            } else if code[1] == 2 {
                                self.relative_base +=
                                    self.intcode[(a + self.relative_base as i64) as usize] as usize;
                            }

                            self.index += 2
                        }
                        _ => (),
                    }
                }
            }
        }
    }
}

fn read_the_damn_intcode(filepath: &'static str) -> Vec<i64> {
    use std::fs::File;
    use std::io::prelude::*;

    let mut file = File::open(filepath).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    contents
        .split(|c| c == ',' || c == ' ')
        .map(|c| c.parse::<i64>().unwrap())
        .collect()
}

fn run(intcode: Vec<i64>, first_color: i64) -> HashMap<Panel, i8> {
    let mut table: HashMap<Panel, i8> = HashMap::new();
    let mut cusor = Panel::new();
    let mut direction = Direction::UP;

    let mut a = Boost::new(&intcode);
    a.run(first_color);
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

        a.run(new_input as i64);
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
    let mut intcode = read_the_damn_intcode("day11.input");
    intcode.append(&mut [0; 10000].to_vec());
    //run(intcode, 0);
    day11_part2(intcode);
}
