use std::fs::File;
use std::io::prelude::*;

fn range_bits(mut data: Vec<u8>, wide: usize, tall: usize) -> Vec<Vec<char>> {
    let mut result: Vec<Vec<char>> = vec![];

    while data.len() != 0 {
        for _ in 0..tall {
            let mut temp = data
                .drain(..wide)
                .map(|c| char::from(c))
                .collect::<Vec<char>>();
            temp.push('\n');
            result.push(temp);
        }
    }

    result
}

fn day8(filepath: String) -> usize {
    let mut file = File::open(filepath).unwrap();
    let mut buffer = [0; 25 * 6];
    let mut buffers: Vec<[u8; 25 * 6]> = vec![];

    loop {
        match file.read_exact(&mut buffer) {
            Ok(_) => buffers.push(buffer),
            Err(_) => break,
        }
    }

    buffers.sort_by(|a, b| {
        a.iter()
            .filter(|b| **b == '0' as u8)
            .count()
            .partial_cmp(&b.iter().filter(|b| **b == '0' as u8).count())
            .unwrap()
    });

    dbg!(buffers[0].iter().filter(|b| **b == '1' as u8).count());
    dbg!(buffers[0].iter().filter(|b| **b == '2' as u8).count());

    buffers[0].iter().filter(|b| **b == '1' as u8).count()
        * buffers[0].iter().filter(|b| **b == '2' as u8).count()
}

fn main() {
    //dbg!(range_bits("123456789012".as_bytes().to_vec(), 3, 2));
    dbg!(day8("./day8.input".to_string()));
}
