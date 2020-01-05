use std::fs::File;
use std::io::prelude::*;
use std::str;

struct InputP {
    count: usize,

    seed: usize,
    seed_count: usize,
}

impl InputP {
    fn new(a: usize) -> Self {
        Self {
            count: a,
            seed: 0,
            seed_count: 0,
        }
    }
}

impl Iterator for InputP {
    type Item = i32;
    fn next(&mut self) -> Option<Self::Item> {
        if self.seed_count == self.count {
            self.seed += 1;
            if self.seed == 4 {
                self.seed = 0
            }
            self.seed_count = 0;
        }
        self.seed_count += 1;
        Some([0, 1, 0, -1][self.seed] as i32)
    }
}

fn char_mult(a: &str, b: &str) -> i32 {
    a.parse::<i32>().unwrap() * b.parse::<i32>().unwrap()
}

fn fft(input: Vec<i32>) -> Vec<i32> {
    let mut this_round = vec![0];
    this_round.append(&mut input.clone());

    let mut result = vec![];

    for ind in 1..this_round.len() {
        let mut p = InputP::new(ind);
        let mut step_sum = 0;
        for num in &this_round {
            let ttt = p.next().unwrap();
            step_sum += num * ttt;
        }

        result.push(step_sum.abs() % 10);
    }

    result
}

// this multi threads version still too slow for part2.
fn fft_a(input: Vec<i32>) -> Vec<i32> {
    use std::thread::*;
    let mut this_round = vec![0];
    this_round.append(&mut input.clone());

    let mut result: Vec<i32> = [0].repeat(this_round.len() - 1);
    let mut joinL: Vec<JoinHandle<i32>> = vec![];

    for ind in 1..this_round.len() {
        let thisthis_round = this_round.clone();
        joinL.push(spawn(move || {
            let mut p = InputP::new(ind.clone());
            let mut step_sum = 0;
            for num in thisthis_round {
                let ttt = p.next().unwrap();
                step_sum += num * ttt;
            }
            step_sum.abs() % 10
        }))
    }

    for i in 0..joinL.len() {
        let a = joinL.remove(0);
        result[i] = a.join().unwrap()
    }

    //result[ind - 1] = step_sum.abs() % 10;
    result
}

fn fft2(input: Vec<i32>) -> Vec<i32> {
    let mut this_round = [0].repeat(input.len());

    let mut ans = 0;
    for i in 1..input.len() / 2 {
        ans += input[input.len() - 2 - i];
        let index = this_round.len() - 1 - i - 1;
        this_round[index as usize] = ans
    }

    this_round = this_round.iter().map(|x| x.abs() % 10).collect();
    this_round
}

fn fft_loop(input: Vec<i32>, time: usize) -> Vec<i32> {
    let mut a = input;
    for i in 0..time {
        a = fft(a)
    }
    a
}

fn str_2_vec(s: &str) -> Vec<i32> {
    s.bytes()
        .map(|b| str::from_utf8(&[b]).unwrap().parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
}

fn day16(filepath: &str) {
    use std::env;
    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();

    //println!("{}", format!("{}/src/{}", path, filepath));
    let mut file = File::open(format!("{}/src/{}", path, filepath)).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    //println!("{:?}", str_2_vec(&contents));
    println!("{:?}", fft_loop(str_2_vec(&contents), 100)); // return all
}

fn day16_part2(filepath: &str) {
    use std::env;
    let path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();

    let mut file = File::open(format!("{}/src/{}", path, filepath)).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    contents = contents.repeat(10000); // 10000 times
    let offset: usize = contents[0..7].parse().unwrap();
    //println!("offset: {}", offset);
    // println!(
    //     "{:?}",
    //     &fft_loop(str_2_vec(&contents), 100)[offset..offset + 8]
    // );
    println!(
        "{:?}",
        &fft_loop(str_2_vec(&contents), 100)[offset + 7..offset + 15]
    );
}

fn main() {
    //day16("day16.input");
    day16_part2("day16.input");
    //println!("{:?}", fft2(vec![1, 2, 3, 4, 5, 6, 7, 8]));
}
