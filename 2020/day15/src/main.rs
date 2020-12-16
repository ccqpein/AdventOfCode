use std::collections::{HashMap, HashSet};
use tools::read_file_by_line;

fn part1(input: &Vec<usize>, end: usize) -> usize {
    let mut table = HashMap::new();
    let mut count = 0;
    let mut last = 0;

    input.iter().for_each(|d| {
        count += 1;
        let a = table.entry(*d).or_insert(vec![]);
        a.push(count);
        last = *d;
    });

    loop {
        count += 1;
        let mut this;
        let en = table.entry(last).or_insert(vec![]);

        if en.len() == 1 {
            let a = table.entry(0).or_insert(vec![]);
            a.push(count);
            this = 0;
        } else {
            let mut b = en.iter().rev().take(2);
            let tt = b.next().unwrap() - b.next().unwrap();
            let b = table.entry(tt).or_insert(vec![]);
            b.push(count);
            this = tt.clone();
        }

        last = this;

        if count == end {
            return last;
        }
    }
}

// fn part2_v2(input: &Vec<usize>, end: usize) -> usize {
//     let mut table = HashMap::new();
//     let mut count = 0;
//     let mut last = 0;

//     let mut set: HashSet<usize> = HashSet::new();
//     let mut records = vec![0];
//     input.iter().for_each(|d| {
//         count += 1;
//         let a = table.entry(*d).or_insert(vec![]);
//         a.push(count);
//         last = *d;
//     });

//     loop {
//         count += 1;
//         let mut this;
//         let en = table.entry(last).or_insert(vec![]);

//         if en.len() == 1 {
//             let a = table.entry(0).or_insert(vec![]);
//             a.push(count);
//             this = 0;
//         } else {
//             let mut b = en.iter().rev().take(2);
//             let tt = b.next().unwrap() - b.next().unwrap();
//             let b = table.entry(tt).or_insert(vec![]);
//             b.push(count);
//             this = tt.clone();
//         }

//         //println!("this: {}", this);
//         // record this step
//         records.push(this);
//         set.insert(this);

//         // find this in set
//         // if let Some(d) = set.get(&this) {
//         //     if *d != 0 {
//         //         let b = table.entry(*d).or_insert(vec![]);
//         //         if b.len() > 4 {
//         //             println!("{:?}", records);
//         //             return 0;
//         //             // let mut b = b.iter().rev().take(2);
//         //             // dbg!(&b);
//         //             // let (this_ind, last_ind) = (b.next().unwrap(), b.next().unwrap());
//         //             // return records[last_ind + (end - last_ind) % (this_ind - last_ind)];
//         //         }
//         //     }
//         // }

//         last = this;

//         if count == end {
//             println!("{:?}", records);
//             return last;
//         }
//     }
// }

fn main() {
    let input = vec![14, 8, 16, 0, 1, 17];
    //let input = vec![0, 3, 6];
    println!("part1: {}", part1(&input, 2020));
    println!("part2: {}", part1(&input, 30000000));
    //println!("part2: {}", part2_v2(&input, 300));
}
