use std::{collections::HashMap, iter};

use itertools::iproduct;
use tools::*;

fn part1(mut p1: i32, mut p2: i32) -> i32 {
    let die = (1..=100).into_iter().collect::<Vec<i32>>().repeat(100);
    let mut die = die.iter();

    let mut die_time = 0;
    let mut p1_score = 0;
    let mut p2_score = 0;
    loop {
        let move_s = die.next().unwrap() + die.next().unwrap() + die.next().unwrap();
        p1 = if (move_s + p1) % 10 == 0 {
            10
        } else {
            (move_s + p1) % 10
        };
        p1_score += p1;
        die_time += 3;
        if p1_score >= 1000 {
            break;
        }

        //////
        let move_s = die.next().unwrap() + die.next().unwrap() + die.next().unwrap();
        p2 = if (move_s + p2) % 10 == 0 {
            10
        } else {
            (move_s + p2) % 10
        };
        p2_score += p2;
        die_time += 3;
        if p2_score >= 1000 {
            break;
        }
    }

    //println!("{},{}", die_time, p2);
    (die_time * p2_score)
}

fn part2(p1: usize, p2: usize) -> (usize, usize) {
    let mut record = HashMap::new();
    helper(&mut record, p1, p2, 0, 0, true)
}

fn helper(
    record: &mut HashMap<(usize, usize, usize, usize, bool), (usize, usize)>,
    p1: usize,
    p2: usize,
    score1: usize,
    score2: usize,
    play1: bool,
) -> (usize, usize) {
    if score1 >= 21 {
        return (1, 0);
    }
    if score2 >= 21 {
        return (0, 1);
    }

    if let Some(score) = record.get(&(p1, p2, score1, score2, play1)) {
        return *score;
    }

    // pick the cache from play2 point of view
    // p2 now is in situation that
    // p1 when in p2 location and have p2 score and p2's turn
    // this optimize only fast a bit
    if let Some(score) = record.get(&(p2, p1, score2, score1, !play1)) {
        return (score.1, score.0);
    }

    let mut score = (0, 0);
    for (d1, d2, d3) in iproduct!([1, 2, 3], [1, 2, 3], [1, 2, 3]) {
        let die_move = d1 + d2 + d3;
        let (s1, s2) = if play1 {
            let next_p1 = if (p1 + die_move) % 10 == 0 {
                10
            } else {
                (p1 + die_move) % 10
            };
            helper(record, next_p1, p2, score1 + next_p1, score2, !play1)
        } else {
            let next_p2 = if (p2 + die_move) % 10 == 0 {
                10
            } else {
                (p2 + die_move) % 10
            };
            helper(record, p1, next_p2, score1, score2 + next_p2, !play1)
        };
        score.0 += s1;
        score.1 += s2;
    }

    record.insert((p1, p2, score1, score2, play1), score);
    score
}

// fn helper(p1: usize, score: usize, next_step: [usize; 3]) -> usize {
//     let mut result = 0;
//     for step in next_step {
//         let next_posiont = if (step + p1) % 10 == 0 {
//             10
//         } else {
//             (step + p1) % 10
//         };
//         let next_score = score + next_posiont;

//         if next_score >= 21 {
//             result += 1
//         } else {
//             // let next_next_step = [
//             //     if next_step[0] + 3 > 100 {
//             //         next_step[0] + 3 - 100
//             //     } else {
//             //         next_step[0] + 3
//             //     },
//             //     if next_step[1] + 3 > 100 {
//             //         next_step[1] + 3 - 100
//             //     } else {
//             //         next_step[1] + 3
//             //     },
//             //     if next_step[2] + 3 > 100 {
//             //         next_step[2] + 3 - 100
//             //     } else {
//             //         next_step[2] + 3
//             //     },
//             // ];
//             let next_next_step = [
//                 if step + 1 > 100 {
//                     step + 1 - 100
//                 } else {
//                     step + 3
//                 },
//                 if step + 2 > 100 {
//                     step + 2 - 100
//                 } else {
//                     step + 2
//                 },
//                 if step + 3 > 100 {
//                     step + 3 - 100
//                 } else {
//                     step + 3
//                 },
//             ];
//             result += helper(next_posiont, next_score, next_next_step);
//         }
//     }
//     return result;
// }

fn main() {
    //let input = read_file_by_line("./src/bin/day21/day21.input");
    //let input = read_file_by_line("./src/bin/day21/day21_demo.input");

    println!("{}", part1(8, 2));
    println!("{:?}", part2(8, 2));
}
