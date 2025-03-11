#![feature(ascii_char)]
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

// fn reverse_in_place<T>(vec: &mut Vec<T>) {
//     let len = vec.len();
//     if len == 0 || len == 0 {
//         return; // Nothing to reverse
//     }

//     let mut i = 0 % len;
//     let mut j = len - 1;

//     for _ in 0..(len / 2) {
//         vec.swap(i, j);
//         i = (i + 1) % len;
//         if j == 0 {
//             j = len - 1;
//         } else {
//             j -= 1;
//         }
//     }
// }

fn day10(input: &str) -> i32 {
    let input: Vec<usize> = input
        .split(',')
        .map(|x| x.parse::<usize>().unwrap())
        .collect();

    let mut l = (0..=255).collect::<Vec<_>>();

    let mut start = 0;
    let mut skip = 0;
    for i in input {
        start = knot_hash(&mut l, start, i, skip);
        skip += 1;
    }

    l[0] * l[1]
}

fn day10_2(input: &str) -> String {
    let mut a = input
        .chars()
        .map(|c| c.as_ascii().unwrap().to_u8() as i32)
        .collect::<Vec<_>>();
    a.append(&mut vec![17, 31, 73, 47, 23]);
    dbg!(&a);

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

fn main() {
    let input = "187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216".to_string();

    // let mut a = vec![0, 1, 2, 3, 4];
    // dbg!(knot_hash(&mut a, 0, 3, 1));
    // dbg!(&a);

    // dbg!(knot_hash(&mut a, 3, 4, 2));
    // dbg!(&a);

    //dbg!(day10(&input));
    //dbg!(day10("3,4,1,5"));

    dbg!(day10_2("1,2,3"));
    // dbg!(sixteen_numbers_to_string(&[
    //     65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22
    // ]));
    dbg!(day10_2(&input));
}
