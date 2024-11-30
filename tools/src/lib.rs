use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::path::Path;

//pub mod custom_carrier;
/// store some useful tool for shortest path
mod graphs;
mod maps;
//mod stack_machine;

pub use maps::*;

pub use graphs::*;

/// read file line by line
pub fn read_file_by_line(filepath: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

/// return same length vec of a, each ind element is a[ind..]
pub fn vec_of_sum_rest<T>(a: &[T], mut f: impl FnMut(T, T) -> T) -> Vec<T>
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_vec_of_sum_rest() {
        let testcase0 = vec![1, 2, 3, 4, 5];
        assert_eq!(vec_of_sum_rest(&testcase0, |a, b| { a + b }), vec![
            15, 14, 12, 9, 5
        ]);

        assert_eq!(vec_of_sum_rest(&[1, 1, 2, 3], |a, b| { a * b }), vec![
            0, 0, 0, 0
        ]);
    }
}
