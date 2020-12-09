use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

pub fn read_file_by_line(filepath: String) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

#[derive(Debug)]
pub struct Ma {
    len: usize,
    status: i32,
    pub boot_code: Vec<(String, i32)>,
    pub accumulator: i32,
}

#[derive(Debug)]
pub struct MaResult {
    pub status: usize,
    pub accumulator: i32,
}

impl Ma {
    pub fn new(ls: Vec<(String, i32)>) -> Self {
        Self {
            len: ls.len(),
            status: 0,
            boot_code: ls,
            accumulator: 0,
        }
    }

    pub fn reset(&mut self) {
        self.status = 0;
        self.accumulator = 0;
    }

    pub fn start_at(&mut self, position: usize) {
        self.status = position as i32;
    }

    pub fn check_status(&mut self) {
        if self.status > self.len as i32 {
            self.status -= self.len as i32;
        } else if self.status < 0 {
            self.status += self.len as i32;
        }
    }

    pub fn write_instruction_at(&mut self, ind: usize, new: String) {
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
