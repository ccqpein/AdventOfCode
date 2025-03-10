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

fn main() {
    let input = "187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216".to_string();

    // let mut a = vec![0, 1, 2, 3, 4];
    // dbg!(knot_hash(&mut a, 0, 3, 1));
    // dbg!(&a);

    // dbg!(knot_hash(&mut a, 3, 4, 2));
    // dbg!(&a);

    //dbg!(day10(&input));
    //dbg!(day10("3,4,1,5"));
}
