use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;

mod maps;
mod stack_machine;

pub fn read_file_by_line(filepath: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

/// return same length vec of a, each ind element is a[ind..]
pub fn vec_of_sum_rest<T>(a: &Vec<T>, mut f: impl FnMut(T, T) -> T) -> Vec<T>
where
    T: Default + Copy,
{
    let mut buffer = Vec::with_capacity(a.len());
    let _ = a.iter().rev().fold(Default::default(), |acc, n| {
        buffer.push(f(acc, *n));
        f(acc, *n)
    });
    buffer.reverse();
    buffer
}
