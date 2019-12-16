#![feature(vec_remove_item)]

fn day5_fix(l: &Vec<i64>, signal: i64, result_from_last: i64) -> i64 {
    let mut index: usize = 0;
    let mut l_inner = l.clone();
    let mut first_time_input = true;

    loop {
        if index >= l_inner.len() {
            break;
        }

        match l_inner[index] {
            1 => {
                let (a, b, c) = (
                    l_inner[index + 1] as usize,
                    l_inner[index + 2] as usize,
                    l_inner[index + 3] as usize,
                );

                l_inner[c] = l_inner[a] + l_inner[b];
                index += 4
            }
            2 => {
                let (a, b, c) = (
                    l_inner[index + 1] as usize,
                    l_inner[index + 2] as usize,
                    l_inner[index + 3] as usize,
                );

                l_inner[c] = l_inner[a] * l_inner[b];
                index += 4
            }
            3 => {
                let a = l_inner[index + 1] as usize;
                if first_time_input {
                    l_inner[a] = signal; // input hard code here
                    first_time_input = false;
                } else {
                    l_inner[a] = result_from_last;
                }
                index += 2
            }
            4 => {
                let a = l_inner[index + 1] as usize;
                return l_inner[a];
                //index += 2
            }

            5 => {
                let (a, b) = (l_inner[index + 1] as usize, l_inner[index + 2] as usize);

                if l_inner[a] != 0 {
                    index = l_inner[b] as usize;
                } else {
                    index += 3;
                }
            }

            6 => {
                let (a, b) = (l_inner[index + 1] as usize, l_inner[index + 2] as usize);

                if l_inner[a] == 0 {
                    index = l_inner[b] as usize;
                } else {
                    index += 3;
                }
            }

            7 => {
                let (a, b, c) = (
                    l_inner[index + 1] as usize,
                    l_inner[index + 2] as usize,
                    l_inner[index + 3] as usize,
                );

                if l_inner[a] < l_inner[b] {
                    l_inner[c] = 1;
                } else {
                    l_inner[c] = 0;
                }

                index += 4;
            }

            8 => {
                let (a, b, c) = (
                    l_inner[index + 1] as usize,
                    l_inner[index + 2] as usize,
                    l_inner[index + 3] as usize,
                );

                if l_inner[a] == l_inner[b] {
                    l_inner[c] = 1;
                } else {
                    l_inner[c] = 0;
                }

                index += 4;
            }

            99 => break,
            opcode @ _ => {
                let (a, b, c) = (
                    l_inner[index + 1] as usize,
                    l_inner[index + 2] as usize,
                    l_inner[index + 3] as usize,
                );

                // code = DE, C, B, A
                let code = vec![
                    opcode % 100,
                    (opcode / 100) % 10,
                    opcode / 1000,
                    opcode / 10000,
                ];
                let (aa, bb, cc);

                match code[0] {
                    1 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }

                        if code[2] == 0 {
                            bb = l_inner[b] as i64
                        } else {
                            bb = b as i64
                        }
                        cc = c;
                        l_inner[cc as usize] = aa + bb;
                        index += 4;
                    }
                    2 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }

                        if code[2] == 0 {
                            bb = l_inner[b] as i64
                        } else {
                            bb = b as i64
                        }
                        cc = c;
                        l_inner[cc as usize] = aa * bb;

                        index += 4;
                    }
                    4 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }
                        return aa;
                        //index += 2;
                    }
                    5 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }

                        if code[2] == 0 {
                            bb = l_inner[b] as i64
                        } else {
                            bb = b as i64
                        }

                        if aa != 0 {
                            index = bb as usize;
                        } else {
                            index += 3
                        }
                    }
                    6 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }

                        if code[2] == 0 {
                            bb = l_inner[b] as i64
                        } else {
                            bb = b as i64
                        }

                        if aa == 0 {
                            index = bb as usize;
                        } else {
                            index += 3
                        }
                    }
                    7 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }

                        if code[2] == 0 {
                            bb = l_inner[b] as i64
                        } else {
                            bb = b as i64
                        }

                        cc = c;

                        if aa < bb {
                            l_inner[cc as usize] = 1;
                        } else {
                            l_inner[cc as usize] = 0;
                        }

                        index += 4;
                    }
                    8 => {
                        if code[1] == 0 {
                            aa = l_inner[a] as i64
                        } else {
                            aa = a as i64
                        }

                        if code[2] == 0 {
                            bb = l_inner[b] as i64
                        } else {
                            bb = b as i64
                        }

                        cc = c;

                        if aa == bb {
                            l_inner[cc as usize] = 1;
                        } else {
                            l_inner[cc as usize] = 0;
                        }

                        index += 4;
                    }
                    _ => (),
                }
            }
        }
    }

    return -100;
}

fn make_all_possibilities(input: Vec<i64>) -> Vec<Vec<i64>> {
    merge_vec(vec![], input)
}

fn merge_vec(head: Vec<i64>, tails: Vec<i64>) -> Vec<Vec<i64>> {
    if tails.len() == 0 {
        return vec![head];
    }

    let mut result = vec![];
    for t in &tails {
        let mut new_tail = tails.clone();
        new_tail.remove_item(t);
        let mut new_head = head.clone();
        new_head.push(*t);
        result.append(&mut merge_vec(new_head, new_tail));
    }

    result
}

fn day7(code: Vec<i64>, input_seed: Vec<i64>) -> i64 {
    let all_inputs = make_all_possibilities(input_seed);

    all_inputs
        .iter()
        .map(|x| {
            let mut last_step_result = 0;
            for i in x {
                last_step_result = day5_fix(&code, *i, last_step_result);
            }
            last_step_result
        })
        .max()
        .unwrap()
}

// fn day7_part2(code: Vec<i64>, input_seed: Vec<i64>) -> i64 {
//     let all_inputs = make_all_possibilities(input_seed);
//     //let all_inputs = vec![input_seed];

//     all_inputs
//         .iter()
//         .map(|x| {
//             let mut last_step_result = 0;
//             let mut result = 0;

//             // first time?
//             for i in x {
//                 result = day5_fix(&code, *i, result);
//                 if result == -100 {
//                     return last_step_result;
//                 }
//                 last_step_result = result;
//             }

//             loop {
//                 result = day5_fix(&code, result, result);
//                 if result == -100 {
//                     println!("hello?");
//                     return last_step_result;
//                 }
//                 last_step_result = result;
//             }
//         })
//         .max()
//         .unwrap()
// }

fn main() {
    //let code_m: Vec<i64> = vec![
    //    3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
    //];

    // {
    //     let mut last_step_result = 0;
    //     for i in vec![4, 3, 2, 1, 0] {
    //         last_step_result = day5_fix(&code_m, i, last_step_result);
    //     }

    //     println!("{:?}", last_step_result);
    // }

    //dbg!(merge_vec(vec![], vec![1, 2, 3]));

    dbg!(day7(
        vec![
            3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1,
            28, 1005, 28, 6, 99, 0, 0, 5
        ],
        vec![5, 6, 7, 8, 9]
    ));
}
