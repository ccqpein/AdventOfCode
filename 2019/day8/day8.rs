use std::fs::File;
use std::io::prelude::*;

fn day8_part2(filepath: String) {
    let mut file = File::open(filepath).unwrap();
    let mut buffer = [0; 25 * 6];
    let mut buffers: Vec<[u8; 25 * 6]> = vec![];

    loop {
        match file.read_exact(&mut buffer) {
            Ok(_) => buffers.push(buffer),
            Err(_) => break,
        }
    }

    //re-use buffer for generate image
    let layer_num = buffers.len();

    for i in 0..25 * 6 {
        for l in 0..layer_num {
            if buffers[l][i] != '2' as u8 {
                if buffers[l][i] == '1' as u8 {
                    buffer[i] = '@' as u8;
                } else {
                    buffer[i] = '.' as u8;
                }
                break;
            }
        }
    }

    let chunks = buffer.chunks(25);
    for i in chunks {
        println!("{}", String::from_utf8(i.to_vec()).unwrap());
    }
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
    //dbg!(day8("./day8.input".to_string()));
    dbg!(day8_part2("./day8.input".to_string()));
}
