use std::collections::HashMap;

use tools::*;

fn part1(input: &Vec<String>) -> usize {
    let mut m = parse_input(input);
    let x_max = input.len() as i32 - 1;
    let y_max = input[0].len() as i32 - 1;
    let mut step = 0;
    loop {
        step += 1;
        let mut new_map = HashMap::new();
        for ((x, y), c) in &m {
            if *c == '>' {
                if let Some((new_x, new_y)) = check_east(&m, (x, y), &(x_max, y_max)) {
                    new_map.insert((new_x, new_y), '>');
                } else {
                    new_map.insert((*x, *y), '>');
                }
            } else {
                new_map.insert((*x, *y), 'v');
            }
        }

        // for x in 0..=x_max {
        //     for y in 0..=y_max {
        //         if let Some(c) = new_map.get(&(x, y)) {
        //             print!("{}", c);
        //         } else {
        //             print!(".")
        //         }
        //     }
        //     println!("")
        // }
        // println!("---");

        let mut new_map_2 = HashMap::new();

        for ((x, y), c) in &new_map {
            if *c == 'v' {
                if let Some((new_x, new_y)) = check_south(&new_map, (x, y), &(x_max, y_max)) {
                    new_map_2.insert((new_x, new_y), 'v');
                } else {
                    new_map_2.insert((*x, *y), 'v');
                }
            } else {
                new_map_2.insert((*x, *y), '>');
            }
        }

        // for x in 0..=x_max {
        //     for y in 0..=y_max {
        //         if let Some(c) = new_map_2.get(&(x, y)) {
        //             print!("{}", c);
        //         } else {
        //             print!(".")
        //         }
        //     }
        //     println!("")
        // }
        // println!("");

        // if step == 4 {
        //     return 0;
        // }

        if new_map_2.eq(&m) {
            return step;
        } else {
            m = new_map_2
        }
    }
}

fn check_east(
    m: &HashMap<(i32, i32), char>,
    (x, y): (&i32, &i32),
    (x_max, y_max): &(i32, i32),
) -> Option<(i32, i32)> {
    if y == y_max {
        if m.contains_key(&(*x, 0)) {
            None
        } else {
            Some((*x, 0))
        }
    } else {
        if m.contains_key(&(*x, y + 1)) {
            None
        } else {
            Some((*x, y + 1))
        }
    }
}
fn check_south(
    m: &HashMap<(i32, i32), char>,
    (x, y): (&i32, &i32),
    (x_max, y_max): &(i32, i32),
) -> Option<(i32, i32)> {
    if x == x_max {
        if m.contains_key(&(0, *y)) {
            None
        } else {
            Some((0, *y))
        }
    } else {
        if m.contains_key(&(x + 1, *y)) {
            None
        } else {
            Some((x + 1, *y))
        }
    }
}

fn parse_input(input: &Vec<String>) -> HashMap<(i32, i32), char> {
    let mut result = HashMap::new();
    for x in 0..input.len() {
        let mut this_l = input[x].chars();
        for y in 0..input[0].len() {
            let this_char = this_l.next().unwrap();
            if this_char == '>' || this_char == 'v' {
                result.insert((x as i32, y as i32), this_char);
            }
        }
    }

    result
}

fn main() {
    let input = read_file_by_line("./src/bin/day25/day25_demo.input");
    let input = read_file_by_line("./src/bin/day25/day25.input");
    println!("{}", part1(&input));
}
