use std::{collections::HashSet, pin::Pin, sync::Arc};

use itertools::Itertools;
use rayon::prelude::*;
use regex::*;
use tokio::task::JoinSet;
use tools::*;

fn parse_input(input: &[String]) -> Vec<(Vec<Vec<usize>>, Vec<i64>)> {
    let re = Regex::new(
        r"^\[.*\]\s*(?P<tuples>(?:\(\d+(?:,\d+)*\)\s*)+)\s*\{\s*(?P<list>\d+(?:,\d+)*)\s*\}$",
    )
    .unwrap();

    let tuple_content_re = Regex::new(r"\((\d+(?:,\d+)*)\)").unwrap();
    let mut res = vec![];
    for line in input {
        match re.captures(line) {
            Some(captures) => {
                let tuples_str = captures.name("tuples").unwrap().as_str();
                let list_str = captures.name("list").unwrap().as_str();

                let mut parsed_tuples: Vec<Vec<usize>> = Vec::new();
                for t_match in tuple_content_re.captures_iter(tuples_str) {
                    let inner_str = t_match.get(1).unwrap().as_str(); // The content inside the parentheses
                    let inner_vec: Vec<usize> = inner_str
                        .split(',')
                        .filter_map(|s| s.trim().parse::<usize>().ok())
                        .collect();
                    parsed_tuples.push(inner_vec);
                }

                let parsed_list: Vec<i64> = list_str
                    .split(',')
                    .filter_map(|s| s.trim().parse::<i64>().ok())
                    .collect();
                res.push((parsed_tuples, parsed_list));
            }
            None => {
                println!("??: {line}")
            }
        }
    }

    res
}

fn press_time_limit(button: &[usize], target: &[i64]) -> usize {
    button.iter().map(|ind| target[*ind]).min().unwrap() as usize
}

fn cal_new_target(button: &[usize], times: usize, target: &[i64]) -> Vec<i64> {
    let mut nt = target.to_vec();
    button.iter().for_each(|ind| nt[*ind] -= times as i64);
    nt
}

fn has_all_button(buttons: &[Vec<usize>], target: &[i64]) -> bool {
    let bbs = buttons
        .iter()
        .flatten()
        .map(|u| *u)
        .collect::<HashSet<usize>>();
    //dbg!(&bbs);
    target
        .iter()
        .enumerate()
        .filter_map(|(ind, n)| if *n > 0 { Some(ind) } else { None })
        .all(|tt| bbs.contains(&tt))
}

fn one_entry(buttons: &[Vec<usize>], target: &[i64]) -> Option<usize> {
    if buttons.len() == 0 {
        return None;
    }
    //dbg!((press_time_limit(buttons.get(0).unwrap(), target)));
    for t in (0..=(press_time_limit(buttons.get(0).unwrap(), target))).rev() {
        let new_target = cal_new_target(buttons.get(0).unwrap(), t, target);
        // dbg!(buttons.get(0));
        // dbg!(&new_target);
        // dbg!(has_all_button(&buttons[1..], &new_target));

        if has_all_button(&buttons[1..], &new_target) && new_target.iter().all(|n| *n >= 0) {
            if new_target.iter().all(|n| *n == 0) {
                return Some(t);
            } else {
                match one_entry(&buttons[1..], &new_target) {
                    Some(n) => return Some(t + n),
                    None => (),
                }
            }
        }
    }
    None
}

// fn one_entry_crazy(
//     buttons_arc: Arc<Vec<Vec<usize>>>, // Now takes an Arc
//     target_arc: Arc<Vec<i64>>,         // Now takes an Arc
// ) -> Pin<Box<dyn Future<Output = Option<usize>> + Send>> {
//     Box::pin(async move {
//         // Dereference Arcs to get slices or references for helper functions
//         let buttons_slice: &Vec<Vec<usize>> = &buttons_arc;
//         let target_slice: &Vec<i64> = &target_arc;

//         if buttons_slice.is_empty() {
//             return None;
//         }

//         let mut set = JoinSet::new();

//         let first_button_ref = buttons_slice.get(0).unwrap();

//         for t in (0..=(press_time_limit(first_button_ref, target_slice))).rev() {
//             // cal_new_target produces a new Vec, so we wrap it in a new Arc
//             let new_target_vec = cal_new_target(first_button_ref, t, target_slice);
//             let new_target_arc_for_spawn = Arc::new(new_target_vec);

//             // Create a Vec for the rest of the buttons to be passed to the next level
//             let rest_buttons_to_spawn_vec = if buttons_slice.len() > 1 {
//                 buttons_slice[1..].to_vec()
//             } else {
//                 vec![]
//             };
//             let rest_buttons_arc_for_spawn = Arc::new(rest_buttons_to_spawn_vec);

//             // helper functions need slices, so dereference the Arcs
//             if has_all_button(&rest_buttons_arc_for_spawn, &new_target_arc_for_spawn)
//                 && new_target_arc_for_spawn.iter().all(|n| *n >= 0)
//             {
//                 if new_target_arc_for_spawn.iter().all(|n| *n == 0) {
//                     return Some(t);
//                 } else {
//                     // Spawn with cloned Arcs
//                     set.spawn(one_entry_crazy(
//                         Arc::clone(&rest_buttons_arc_for_spawn),
//                         Arc::clone(&new_target_arc_for_spawn),
//                     ));
//                 }
//             }
//         }

//         let mut min_val_found: Option<usize> = None;
//         let results = set.join_all().await; // Await point

//         for res in results {
//             match res {
//                 Some(n) => {
//                     // The logic here remains as per your original intent for combining results.
//                     // This 'n' is the result from a child call. If the goal is to find minimal 't + n',
//                     // then `t` from the current loop iteration needs to be incorporated.
//                     // For simply passing through the child's 'n' as a candidate:
//                     if min_val_found.is_none() || Some(n) < min_val_found {
//                         min_val_found = Some(n);
//                     }
//                 }
//                 None => {} // Child branch didn't find a solution
//             }
//         }
//         min_val_found
//     })
// }

fn day10_2(input: &[String]) -> usize {
    let mut input = parse_input(input);

    let sum = input
        .par_iter_mut()
        .map(|(buttons, target)| {
            buttons.sort_by(|a, b| b.len().cmp(&a.len()));
            let x = one_entry(&buttons, &target).unwrap();
            println!("get result: {x}");
            x
        })
        .sum();
    sum
}

// async fn day10_2_crazy(input: &[String]) -> usize {
//     let parsed_input = parse_input(input);
//     let mut join_set = JoinSet::new(); // Use JoinSet to manage spawned tasks

//     println!("Spawning tasks for day10_2_crazy...");
//     for (buttons_data, target_data) in parsed_input {
//         // Wrap data in Arc for thread-safe passing to spawned tasks
//         let buttons_arc = Arc::new(buttons_data);
//         let target_arc = Arc::new(target_data);

//         // Spawn one_entry_crazy for each line
//         join_set.spawn(one_entry_crazy(buttons_arc, target_arc));
//     }

//     let mut total_res = 0usize;
//     println!("Waiting for all day10_2_crazy tasks to complete...");

//     // Await all tasks in the JoinSet.
//     // .join_all() waits for all currently spawned tasks to complete,
//     // and returns their results. It yields a Vec<Result<Output, JoinError>>.
//     let results = join_set.join_all().await;

//     for result in results {
//         match result {
//             // If the task completed successfully and returned a Some(value)
//             Some(n) => {
//                 total_res += n;
//             }
//             // If the task completed successfully but returned None
//             None => {
//                 eprintln!(
//                     "Warning: one_entry_crazy task returned None (no solution found for an entry)."
//                 );
//                 // Depending on requirements, you might want to panic, return an Err, or handle it differently.
//                 // For 'unwrap()' behavior, we'll treat None as 0 or panic.
//                 // For this example, we'll continue, effectively treating None as 0 in the sum.
//             }
//         }
//     }

//     total_res
// }

fn day10_2_crazy_af(input: &str) -> usize {
    use arrayvec::ArrayVec;
    use atoi;
    use microlp::{LinearExpr, OptimizationDirection, Problem};

    const BTNS_SIZE: usize = 13;
    const JOLT_SIZE: usize = 10;

    //dbg!(&input);

    let presses = input
        .as_bytes()
        .split(|&b| b == b'\n')
        .map(|line| {
            // dbg!(line);
            // dbg!(b'{');
            // dbg!(&line.iter().position(|&b| b == b'{').unwrap());
            let (first, last) = line.split_at(line.iter().position(|&b| b == b'{').unwrap());

            let btns = first[1..]
                .split(|&b| b == b' ')
                .skip(1)
                .filter(|btns| !btns.is_empty())
                .map(|btns| {
                    btns[1..]
                        .split(|&b| b == b',')
                        .map(|n| 1 << (n[0] - b'0'))
                        .sum()
                })
                .collect::<ArrayVec<u16, BTNS_SIZE>>();
            let jolts = last[1..]
                .split(|&b| b == b',')
                .map(|b| atoi::atoi::<u16>(b).unwrap())
                .collect::<ArrayVec<u16, JOLT_SIZE>>();

            let mut problem = Problem::new(OptimizationDirection::Minimize);
            let max = jolts.iter().copied().max().unwrap();
            let vars = (0..btns.len())
                .map(|_| problem.add_integer_var(1.0, (0, max as i32)))
                .collect::<ArrayVec<_, BTNS_SIZE>>();
            for (i, &n) in jolts.iter().enumerate() {
                problem.add_constraint(
                    btns.iter()
                        .zip(&vars)
                        .filter(|&(mask, _)| mask & (1 << i) != 0)
                        .fold(LinearExpr::empty(), |mut ex, (_, &var)| {
                            ex.add(var, 1.0);
                            ex
                        }),
                    microlp::ComparisonOp::Eq,
                    n as f64,
                );
            }
            problem.solve().unwrap().objective().round() as usize
        })
        .sum::<usize>();
    presses
}

fn main() {
    let input = read_file_by_line("../inputs/day10.input");
    //let input = read_file_by_line("../inputs/day10_demo.input");
    //dbg!(day4(&input));
    //dbg!(day10_2(&input[0..1]));
    //dbg!(day10_2(&input));
    //dbg!(day10_2_crazy(&input).await);
    dbg!(day10_2_crazy_af(include_str!(
        "../../../inputs/day10.input"
    )));

    // dbg!(one_entry(
    //     &vec![
    //         vec![3],
    //         vec![1, 3],
    //         vec![2],
    //         vec![2, 3],
    //         vec![0, 2],
    //         vec![0, 1],
    //     ],
    //     &vec![0, 1, 0, 2],
    // ));
}
