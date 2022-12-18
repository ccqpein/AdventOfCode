#![feature(int_roundings)]

use lazy_static::*;
use std::collections::VecDeque;
use tools::*;

#[derive(Clone, Debug)]
struct Rock {
    nums: Vec<u8>,
    max: Vec<u8>,
    min: Vec<u8>,
}

impl Rock {
    fn shift(&mut self, command: char) {
        match command {
            '>' => {
                let mut flag = false;
                for (ind, n) in self.nums.iter().enumerate() {
                    if n >> 1 < self.min[ind] {
                        flag = true || flag
                    }
                }

                if flag {
                    return;
                } else {
                    for n in &mut self.nums {
                        *n >>= 1
                    }
                }
            }
            '<' => {
                let mut flag = false;
                for (ind, n) in self.nums.iter().enumerate() {
                    if n << 1 > self.max[ind] {
                        flag = true || flag
                    }
                }

                if flag {
                    return;
                } else {
                    for n in &mut self.nums {
                        *n <<= 1
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn has_coverd(&self, buffer: &VecDeque<u8>) -> bool {
        for (idx, n) in self.nums.iter().enumerate() {
            if let Some(m) = buffer.get(idx) {
                if has_covered(*m, *n) {
                    return true;
                }
            } else {
                return false;
            }
        }
        false
    }

    fn and_buffer(&self, buffer: &Vec<u8>) -> Vec<u8> {
        let mut result = vec![];
        for (idx, n) in self.nums.iter().enumerate() {
            if let Some(m) = buffer.get(idx) {
                result.push(*n | *m)
            } else {
                result.push(*n)
            }
        }
        if buffer.len() > self.nums.len() {
            for idx in self.nums.len()..buffer.len() {
                result.push(buffer[idx])
            }
        }
        result
    }

    fn shift_with_buffer(&mut self, command: char, buffer: &VecDeque<u8>) {
        let mut clone_self = self.clone();
        clone_self.shift(command);

        if !clone_self.has_coverd(buffer) {
            *self = clone_self
        }
    }
}

lazy_static! {
    static ref ROCKS: Vec<Rock> = vec![
        Rock {
            nums: vec![0b11110],
            max: vec![0b1111000],
            min: vec![0b1111],
        },
        Rock {
            nums: vec![0b1000, 0b11100, 0b1000],
            max: vec![0b100000, 0b1110000, 0b100000],
            min: vec![0b10, 0b111, 0b10],
        },
        Rock {
            nums: vec![0b11100, 0b100, 0b100],
            max: vec![0b1110000, 0b10000, 0b10000],
            min: vec![0b111, 0b1, 0b1],
        },
        Rock {
            nums: vec![0b10000, 0b10000, 0b10000, 0b10000],
            max: vec![0b1000000, 0b1000000, 0b1000000, 0b1000000],
            min: vec![0b1, 0b1, 0b1, 0b1],
        },
        Rock {
            nums: vec![0b11000, 0b11000],
            max: vec![0b1100000, 0b1100000],
            min: vec![0b11, 0b11],
        },
    ];
}

fn part1(inputs: &Vec<String>, rock_num: usize) -> usize {
    let mut inputs = inputs[0].chars().cycle();
    let mut tower = VecDeque::new();
    let mut rocks = ROCKS.iter().cycle();

    let mut buffer = VecDeque::new();
    let mut count_record = 0;
    for rn in 0..rock_num {
        let mut this: Rock = rocks.next().unwrap().clone();
        for _ in 0..4 {
            this.shift(inputs.next().unwrap())
        }

        loop {
            if let Some(r) = tower.pop_front() {
                if r == 127 {
                    count_record += tower.len();
                    //println!("tower.len(): {}, this rock: {}", tower.len(), rn);
                    tower.clear();
                }
                buffer.push_front(r);
                if this.has_coverd(&buffer) {
                    tower.push_front(buffer[0]);
                    for n in this.and_buffer(&buffer.range(1..).cloned().collect()) {
                        tower.push_front(n)
                    }
                    break;
                } else {
                    this.shift_with_buffer(inputs.next().unwrap(), &buffer);
                }
            } else {
                for n in this.and_buffer(&buffer.clone().into()) {
                    //println!("n: {}", n);
                    tower.push_front(n);
                }
                break;
            }
        }
        buffer.clear()
    }
    println!(
        "tower len inside and record: {}, {}",
        tower.len(),
        count_record
    );
    tower.len() + count_record
}

fn has_covered(a: u8, b: u8) -> bool {
    !(a | b == a + b)
}

fn print_out(x: &Vec<u8>) {
    for xx in x.iter().rev() {
        println!("|{:07b}|", xx)
    }
}

fn part2(
    inputs: &Vec<String>,
    rock_num: usize,
    command_offset: usize,
    rock_offset: usize,
    init_buffer: Option<Vec<u8>>,
) -> usize {
    let mut inputs = inputs[0].chars().cycle();
    let mut tower = {
        if let Some(mut a) = init_buffer {
            a.reverse();
            a.into_iter().collect()
        } else {
            VecDeque::new()
        }
    };

    let mut rocks = ROCKS.iter().cycle();

    for _ in 0..command_offset {
        inputs.next();
    }

    for _ in 0..rock_offset {
        rocks.next();
    }

    let mut commands_count = 0;
    let mut total_rock_num = 0;
    let mut record = 0;
    let mut rock_record = 0;

    let mut buffer = VecDeque::new();
    let mut this_rock_command_count = 0;
    loop {
        if total_rock_num + rock_record == rock_num {
            // println!(
            //     "tower len inside, record, and rocks: {}, {}, {}",
            //     tower.len(),
            //     record,
            //     total_rock_num + rock_record
            // );

            let result = tower.len() + record;
            //print_out(&tower.into_iter().rev().collect());
            return result;
        }

        let mut this: Rock = rocks.next().unwrap().clone();

        for _ in 0..4 {
            this.shift(inputs.next().unwrap());
            commands_count += 1;
            this_rock_command_count += 1
        }

        loop {
            if let Some(r) = tower.get(0) {
                if *r == 127 {
                    println!(
                        "tower_len: {},total_rock_num: {},command_count before this rock: {},this rock is {}
buffer: {:?}, this rock: {:?}",
                        tower.len(),
                        total_rock_num,
                        commands_count - this_rock_command_count,
                        (rock_record + total_rock_num) % 5,
						buffer,
						this
                    );

                    //print_out(&this.and_buffer(&buffer.clone().into()));

                    record += tower.len();
                    rock_record += total_rock_num;
                    tower.clear();
                    total_rock_num = 0;
                }
            }

            if let Some(r) = tower.pop_front() {
                buffer.push_front(r);
                if this.has_coverd(&buffer) {
                    tower.push_front(buffer[0]);
                    for n in this.and_buffer(&buffer.range(1..).cloned().collect()) {
                        tower.push_front(n)
                    }
                    break;
                } else {
                    this.shift_with_buffer(inputs.next().unwrap(), &buffer);
                    commands_count += 1;
                    this_rock_command_count += 1
                }
            } else {
                for n in this.and_buffer(&buffer.clone().into()) {
                    //println!("n: {}", n);
                    tower.push_front(n);
                }
                break;
            }
        }

        total_rock_num += 1;
        buffer.clear();
        this_rock_command_count = 0;
    }
}

fn main() {
    let input = read_file_by_line("./inputs/day17.input");
    println!("part1: {:?}\n", part1(&input, 2022));

    // println!("part1: {:?}\n", part2(&input, 419, 0, 0, None));

    // println!(
    //     "part1: {:?}\n",
    //     part2(
    //         &input,
    //         1,
    //         2435,
    //         3,
    //         Some(vec![16, 16, 24, 28, 15, 1, 1].into())
    //     ) + 627
    // );

    // for get the repeat pattern
    // println!("part1 5000: {:?}\n", part2(&input, 5000, 0, 0, None));
    // let repeat = (5000 - 418) / (300 + 573 + 66 + 806);
    // println!(
    //     "part1 5000: {:?}\n",
    //     part2(
    //         &input,
    //         //(2022 - 418) % (300 + 573 + 66),
    //         (5000 - 418) % (300 + 573 + 66 + 806),
    //         12526,
    //         3,
    //         Some(vec![16, 16, 24, 28, 15, 1, 1])
    //     ) + 627
    //         + (492 + 922 + 108 + 1256) * repeat
    // );

    let repeat = (2022 - 418) / (300 + 573 + 66);
    println!("repeat: {}", repeat);
    println!(
        "part1 again: {:?}\n",
        part2(
            &input,
            //(2022 - 418) % (300 + 573 + 66),
            (2022 - 418) % (300 + 573 + 66),
            7816,
            2,
            Some(vec![2].into())
        ) + 627
            + (492 + 922 + 108) * repeat
    );

    let repeat = (1000000000000 - 418) / (300 + 573 + 66 + 806);
    //println!("repeat: {}", repeat);
    println!(
        "part2: {:?}",
        part2(
            &input,
            (1000000000000 - 418) % (300 + 573 + 66 + 806),
            12526,
            3,
            Some(vec![16, 16, 24, 28, 15, 1, 1])
        ) + 627
            + (492 + 922 + 108 + 1256) * repeat
    );
}
