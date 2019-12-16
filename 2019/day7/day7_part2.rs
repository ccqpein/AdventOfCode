#![feature(vec_remove_item)]
/// part 2 is weird, I re-write it now
#[derive(Debug)]
enum Status {
    Halt,
    Waiting,
}

#[derive(Debug)]
struct Amplifier {
    intcode: Vec<i64>,
    index: usize, // where is stop
    status: Status,
    output: i64,
    input: Option<i64>,
}

impl Amplifier {
    fn new(intcode: &Vec<i64>) -> Self {
        Amplifier {
            status: Status::Waiting,
            index: 0,
            output: 0,
            intcode: intcode.clone(),
            input: None,
        }
    }

    fn input(&mut self, input: i64) -> Status {
        self.run_with_input(input)
    }

    fn run_with_input(&mut self, input: i64) -> Status {
        self.run(Some(input))
    }

    fn start(&mut self, input1: i64, input2: i64) -> Status {
        self.run(Some(input1));
        self.run(Some(input2))
    }

    fn start_with_init(&mut self, input: i64) {
        self.run(Some(input));
    }

    fn run(&mut self, input: Option<i64>) -> Status {
        self.input = input;
        loop {
            if self.index >= self.intcode.len() {
                self.status = Status::Halt;
                return Status::Halt;
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
                    let a = self.intcode[self.index + 1] as usize;
                    if let Some(inp) = self.input {
                        self.intcode[a] = inp;
                        self.input = None;
                    } else {
                        self.status = Status::Waiting;
                        return Status::Waiting;
                    }

                    self.index += 2
                }
                4 => {
                    let a = self.intcode[self.index + 1] as usize;
                    self.output = self.intcode[a];
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

                99 => {
                    self.status = Status::Halt;
                    return Status::Halt;
                }
                opcode @ _ => {
                    let (a, b, c) = (
                        self.intcode[self.index + 1] as usize,
                        self.intcode[self.index + 2] as usize,
                        self.intcode[self.index + 3] as usize,
                    );

                    // code = DE, C, B, A
                    let code = vec![
                        opcode % 100,
                        (opcode / 100) % 10,
                        opcode / 1000,
                        opcode / 10000,
                    ];
                    let (aa, bb, cc);

                    match code[0] {
                        1 => {
                            if code[1] == 0 {
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b] as i64
                            } else {
                                bb = b as i64
                            }
                            cc = c;
                            self.intcode[cc as usize] = aa + bb;
                            self.index += 4;
                        }
                        2 => {
                            if code[1] == 0 {
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b] as i64
                            } else {
                                bb = b as i64
                            }
                            cc = c;
                            self.intcode[cc as usize] = aa * bb;

                            self.index += 4;
                        }
                        4 => {
                            if code[1] == 0 {
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }
                            self.output = aa;
                            self.index += 2;
                        }
                        5 => {
                            if code[1] == 0 {
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b] as i64
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
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b] as i64
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
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b] as i64
                            } else {
                                bb = b as i64
                            }

                            cc = c;

                            if aa < bb {
                                self.intcode[cc as usize] = 1;
                            } else {
                                self.intcode[cc as usize] = 0;
                            }

                            self.index += 4;
                        }
                        8 => {
                            if code[1] == 0 {
                                aa = self.intcode[a] as i64
                            } else {
                                aa = a as i64
                            }

                            if code[2] == 0 {
                                bb = self.intcode[b] as i64
                            } else {
                                bb = b as i64
                            }

                            cc = c;

                            if aa == bb {
                                self.intcode[cc as usize] = 1;
                            } else {
                                self.intcode[cc as usize] = 0;
                            }

                            self.index += 4;
                        }
                        _ => (),
                    }
                }
            }
        }
    }
}

fn make_all_possibilities(input: Vec<i64>) -> Vec<Vec<i64>> {
    merge_vec(vec![], input)
}

fn merge_vec(head: Vec<i64>, tails: Vec<i64>) -> Vec<Vec<i64>> {
    if tails.len() == 0 {
        return vec![head];
    }

    let mut result = vec![];
    for t in &tails {
        let mut new_tail = tails.clone();
        new_tail.remove_item(t);
        let mut new_head = head.clone();
        new_head.push(*t);
        result.append(&mut merge_vec(new_head, new_tail));
    }

    result
}

fn day7_part2(ic: Vec<i64>, input_seed: Vec<i64>) -> i64 {
    let all_inputs = make_all_possibilities(input_seed);

    all_inputs
        .iter()
        .map(|x| {
            let mut amp1 = Amplifier::new(&ic);
            let mut amp2 = Amplifier::new(&ic);
            let mut amp3 = Amplifier::new(&ic);
            let mut amp4 = Amplifier::new(&ic);
            let mut amp5 = Amplifier::new(&ic);

            amp1.start_with_init(x[0]);
            amp2.start_with_init(x[1]);
            amp3.start_with_init(x[2]);
            amp4.start_with_init(x[3]);
            amp5.start_with_init(x[4]);

            let mut amps: Vec<Amplifier> = vec![amp1, amp2, amp3, amp4, amp5];
            let mut input_cache = 0;
            //let mut output = 0;
            for i in vec![0, 1, 2, 3, 4].iter().cycle() {
                match amps[*i as usize].run_with_input(input_cache) {
                    Status::Waiting => {
                        input_cache = amps[*i as usize].output;
                    }
                    Status::Halt => {
                        if *i == 4 {
                            break;
                        }
                        input_cache = amps[*i as usize].output;
                    }
                }
            }

            amps[4].output
        })
        .max()
        .unwrap()
}

fn main() {
    let ic = vec![
        3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5,
        54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53,
        1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
    ];

    //println!("{:?}", day7_part2(ic, vec![9, 8, 7, 6, 5]));

    let ic2 = vec![
        3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28,
        1005, 28, 6, 99, 0, 0, 5,
    ];

    //println!("{:?}", day7_part2(ic2, vec![9, 8, 7, 6, 5]));

    let ic3 = vec![
        3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 38, 55, 64, 89, 114, 195, 276, 357, 438, 99999, 3,
        9, 101, 3, 9, 9, 102, 3, 9, 9, 1001, 9, 5, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 1002, 9, 3, 9,
        101, 5, 9, 9, 4, 9, 99, 3, 9, 101, 3, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 4, 9, 101, 5, 9, 9,
        1002, 9, 5, 9, 101, 5, 9, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 101, 3, 9, 9, 1002, 9, 4, 9,
        101, 5, 9, 9, 102, 5, 9, 9, 1001, 9, 5, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101,
        2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4,
        9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9,
        1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001,
        9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4,
        9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9,
        102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 101, 2,
        9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9,
        3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001,
        9, 1, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2,
        9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3,
        9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9,
        2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9,
        4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9,
        101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9,
        9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 99,
    ];

    println!("{:?}", day7_part2(ic3, vec![9, 8, 7, 6, 5]));
}
