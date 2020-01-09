#[derive(Debug)]
pub struct Intcode {
    intcode: Vec<i64>,
    pub output: Vec<i64>,
    relative_base: usize,
    pub index: usize,
    pub input: Vec<i64>,
    pub status: String,
}

impl Intcode {
    pub fn new(intcode: &Vec<i64>) -> Self {
        Intcode {
            intcode: intcode.clone(),
            output: vec![],
            relative_base: 0,
            index: 0,
            input: vec![],
            status: String::new(),
        }
    }

    pub fn clean_input(&mut self) {
        self.input = vec![];
    }

    pub fn run(&mut self, input: Option<i64>) {
        if let Some(v) = input {
            self.input.push(v)
        };

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
                        self.status = "Waiting".to_string();
                        return;
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
                            if self.input.len() == 0 {
                                self.status = "Waiting".to_string();
                                return;
                            }

                            let input = self.input[0];
                            self.input.drain(..1);

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

pub fn read_the_damn_intcode(filepath: &str) -> Vec<i64> {
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
