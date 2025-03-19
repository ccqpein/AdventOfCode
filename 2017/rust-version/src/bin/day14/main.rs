#![feature(ascii_char)]
use tools::*;

fn knot_hash(l: &mut Vec<i32>, start: usize, len: usize, skip: usize) -> usize {
    //dbg!(start);
    let indexes = {
        (0..len)
            .into_iter()
            .map(|offset| (start + offset) % l.len())
            .collect::<Vec<_>>()
    };
    //dbg!(&indexes);
    let mut values: Vec<i32> = indexes.iter().map(|ind| l[*ind]).collect();
    //reverse_in_place(&mut values);
    values.reverse();
    for ind in 0..indexes.len() {
        l[indexes[ind]] = values[ind]
    }

    (start + skip + len) % l.len()
}

fn calculate_hash(input: &str) -> String {
    let mut a = input
        .chars()
        .map(|c| c.as_ascii().unwrap().to_u8() as i32)
        .collect::<Vec<_>>();
    a.append(&mut vec![17, 31, 73, 47, 23]);

    let mut l = (0..=255).collect::<Vec<_>>();
    let mut start = 0;
    let mut skip = 0;

    for _ in 0..64 {
        for i in &a {
            start = knot_hash(&mut l, start, *i as usize, skip);
            skip += 1;
        }
    }

    let mut re = String::new();
    for i in 0..16 {
        re += sixteen_numbers_to_string(l.get(i * 16..i * 16 + 16).unwrap()).as_str()
    }

    re
}

fn sixteen_numbers_to_string(h: &[i32]) -> String {
    format!("{:02x}", h.into_iter().fold(0, |init, n| init ^ n))
}

fn hex_char_to_binary(hex_char: char) -> (String, u32) {
    match hex_char {
        '0' => ("0000".to_string(), 0),
        '1' => ("0001".to_string(), 1),
        '2' => ("0010".to_string(), 2),
        '3' => ("0011".to_string(), 3),
        '4' => ("0100".to_string(), 4),
        '5' => ("0101".to_string(), 5),
        '6' => ("0110".to_string(), 6),
        '7' => ("0111".to_string(), 7),
        '8' => ("1000".to_string(), 8),
        '9' => ("1001".to_string(), 9),
        'a' | 'A' => ("1010".to_string(), 10),
        'b' | 'B' => ("1011".to_string(), 11),
        'c' | 'C' => ("1100".to_string(), 12),
        'd' | 'D' => ("1101".to_string(), 13),
        'e' | 'E' => ("1110".to_string(), 14),
        'f' | 'F' => ("1111".to_string(), 15),
        _ => unreachable!(),
    }
}

fn day14(input: &str) -> u32 {
    let mut re = 0;
    for i in 0..128 {
        for c in calculate_hash(&format!("{}-{}", input, i)).chars() {
            //dbg!((c as u32));
            re += hex_char_to_binary(c).1.count_ones()
        }
    }
    re
}

fn day14_2(input: &str) -> usize {
    let mut lines = vec![];
    for i in 0..128 {
        let mut a = vec![];
        for c in calculate_hash(&format!("{}-{}", input, i)).chars() {
            a.append(&mut hex_char_to_binary(c).0.chars().collect());
        }
        lines.push(a);
    }
    //dbg!(&lines);
    let m: Map<char> = lines.into();
    let sgs = Segment::gen_segments(&m);
    let mut count = 0;
    for s in sgs {
        if *s.value() == '1' {
            count += 1;
        }
    }

    count
}

fn main() {
    //let input = "jxqlasbh";
    //dbg!(calculate_hash("flqrgnkx-0"));
    //println!("{:x}", "d");
    //dbg!(hex_char_to_binary('d'));
    //dbg!(13_i32.count_ones());
    //dbg!(day14("flqrgnkx"));

    //dbg!(day14(input));
    //let input = "flqrgnkx";
    let input = "jxqlasbh";
    dbg!(day14_2(input));
}
