use std::collections::HashSet;
use tools::read_file_by_line;

#[derive(Debug)]
struct Ma {
    len: usize,
    status: i32,
    boot_code: Vec<(String, i32)>,
    accumulator: i32,
}

#[derive(Debug)]
struct MaResult {
    status: usize,
    accumulator: i32,
}

impl Ma {
    fn new(ls: Vec<(String, i32)>) -> Self {
        Self {
            len: ls.len(),
            status: 0,
            boot_code: ls,
            accumulator: 0,
        }
    }

    fn reset(&mut self) {
        self.status = 0;
        self.accumulator = 0;
    }

    fn start_at(&mut self, position: usize) {
        self.status = position as i32;
    }

    fn check_status(&mut self) {
        if self.status > self.len as i32 {
            self.status -= self.len as i32;
        } else if self.status < 0 {
            self.status += self.len as i32;
        }
    }

    fn write_instruction_at(&mut self, ind: usize, new: String) {
        self.boot_code[ind] = (new, self.boot_code[ind].1)
    }
}

impl Iterator for Ma {
    type Item = MaResult;
    fn next(&mut self) -> Option<Self::Item> {
        if self.status == self.len as i32 {
            return None;
        }
        let this = self.boot_code[self.status as usize].clone();
        //println!("{:?}, {}", this, self.status);
        match this.0.as_str() {
            "nop" => {
                self.status += 1;
                self.check_status();
            }
            "acc" => {
                self.accumulator += this.1;
                self.status += 1;
                self.check_status();
            }
            "jmp" => {
                self.status += this.1;
                self.check_status();
            }
            _ => {}
        }

        Some(MaResult {
            status: self.status as usize,
            accumulator: self.accumulator,
        })
    }
}

fn part1(ma: &mut Ma) -> MaResult {
    let mut set = HashSet::new();
    for status in ma {
        if set.contains(&status.status) {
            return status;
        }
        set.insert(status.status);
    }

    MaResult {
        status: 0,
        accumulator: 0,
    }
}

fn part2(ma: &mut Ma) -> i32 {
    let all_nop = ma
        .boot_code
        .iter()
        .enumerate()
        .filter(|(_, a)| a.0 == "nop")
        .map(|(ind, _)| ind)
        .collect::<Vec<usize>>();

    let all_jmp = ma
        .boot_code
        .iter()
        .enumerate()
        .filter(|(_, a)| a.0 == "jmp")
        .map(|(ind, _)| ind)
        .collect::<Vec<usize>>();

    for n in all_nop {
        ma.reset();
        ma.write_instruction_at(n, "jmp".to_string());
        if part1(ma).status == 0 {
            return ma.accumulator;
        }
        ma.write_instruction_at(n, "nop".to_string());
    }

    for n in all_jmp {
        ma.reset();
        ma.write_instruction_at(n, "nop".to_string());
        if part1(ma).status == 0 {
            return ma.accumulator;
        }
        ma.write_instruction_at(n, "jmp".to_string());
    }

    -1
}

fn main() {
    let input = read_file_by_line(String::from("./src/day8.input"));
    let input = input
        .iter()
        .map(|line| {
            let mut a = line.split_whitespace();
            (
                a.next().unwrap().to_string(),
                a.next().unwrap().parse::<i32>().unwrap(),
            )
        })
        .collect::<Vec<(String, i32)>>();

    let mut ma = Ma::new(input);
    println!("{:?}", part1(&mut ma));
    ma.reset();
    println!("{:?}", part2(&mut ma));
}
