use std::collections::HashSet;

use tools::*;

fn part1(input: &Vec<String>) -> usize {
    let mut on_set = HashSet::new();

    for line in input {
        let (comm, x, y, z) = parse_input(line);
        let new_x_range = helper(x, vec![-50, 50]);
        let new_y_range = helper(y, vec![-50, 50]);
        let new_z_range = helper(z, vec![-50, 50]);

        if comm == "on" {
            for xx in new_x_range[0]..=new_x_range[1] {
                for yy in new_y_range[0]..=new_y_range[1] {
                    for zz in new_z_range[0]..=new_z_range[1] {
                        on_set.insert((xx, yy, zz));
                    }
                }
            }
        } else {
            for xx in new_x_range[0]..=new_x_range[1] {
                for yy in new_y_range[0]..=new_y_range[1] {
                    for zz in new_z_range[0]..=new_z_range[1] {
                        on_set.remove(&(xx, yy, zz));
                    }
                }
            }
        }
    }

    on_set.len()
}

fn helper(range: Vec<i32>, target: Vec<i32>) -> Vec<i32> {
    let mut new_range = vec![];
    if range[0] < target[0] {
        new_range.push(target[0]);
    } else {
        new_range.push(range[0]);
    }

    if range[1] > target[1] {
        new_range.push(target[1]);
    } else {
        new_range.push(range[1]);
    }

    new_range
}

fn part2(input: &Vec<String>) -> usize {
    let mut on_x_bucket = vec![];
    let mut on_y_bucket = vec![];
    let mut on_z_bucket = vec![];

    let mut off_x_bucket = vec![];
    let mut off_y_bucket = vec![];
    let mut off_z_bucket = vec![];

    for line in input {
        let (comm, x, y, z) = parse_input(line);
        if comm == "on" {
            on_x_bucket = helper2(on_x_bucket, &x);
            on_y_bucket = helper2(on_y_bucket, &y);
            on_z_bucket = helper2(on_z_bucket, &z);
        } else {
            off_x_bucket = helper2(off_x_bucket, &x);
            off_y_bucket = helper2(off_y_bucket, &y);
            off_z_bucket = helper2(off_z_bucket, &z);
        }
    }

    let mut on_set = HashSet::new();
    for xx in &on_x_bucket {
        for x in xx[0]..=xx[1] {
            for yy in &on_y_bucket {
                for y in yy[0]..=yy[1] {
                    for zz in &on_z_bucket {
                        for z in zz[0]..=zz[1] {
                            on_set.insert((x, y, z));
                        }
                    }
                }
            }
        }
    }

    for xx in &off_x_bucket {
        for x in xx[0]..=xx[1] {
            for yy in &off_y_bucket {
                for y in yy[0]..=yy[1] {
                    for zz in &off_z_bucket {
                        for z in zz[0]..=zz[1] {
                            on_set.remove(&(x, y, z));
                        }
                    }
                }
            }
        }
    }

    on_set.len()
}

fn helper2(bucket: Vec<Vec<i32>>, new_member: &Vec<i32>) -> Vec<Vec<i32>> {
    let mut result = vec![];
    let mut done = false;
    for range in bucket {
        if done {
            result.push(range);
            continue;
        }

        if new_member[0] < range[1] && new_member[1] > range[1] {
            if new_member[0] > range[0] {
                result.push(vec![range[0], new_member[1]]);
                done = true;
            } else {
                result.push(new_member.clone());
                done = true;
            }
        } else if new_member[0] < range[1] && new_member[1] < range[1] {
            if new_member[0] < range[0] {
                result.push(vec![new_member[0], range[1]]);
                done = true;
            } else {
                result.push(range);
                done = true;
            }
        } else {
            result.push(range)
        }
    }

    if !done {
        result.push(new_member.clone());
    }

    result
}

fn part2_v2(input: &Vec<String>) {
    dbg!(input.len());
    let mut on_bucket = vec![];
    let mut off_bucket = vec![];
    let cubes = input.iter().for_each(|line| {
        let (comm, x, y, z) = parse_input(line);
        if comm == "on" {
            on_bucket.push(vec![x, y, z]);
        } else {
            off_bucket.push(vec![x, y, z])
        }
    });

    let mut on_and_off: Vec<(usize, Vec<usize>)> = vec![];

    for (onidx, on) in on_bucket.iter().enumerate() {
        let mut cache = vec![];
        for (offidx, off) in off_bucket.iter().enumerate() {
            if overlap_check(on.to_vec(), off.to_vec()) {
                cache.push(offidx);
            }
        }
        on_and_off.push((onidx, cache));
    }

    dbg!(on_bucket.len());
    dbg!(off_bucket.len());
    dbg!(on_and_off);
}

fn on_minus_off(on: &Vec<Vec<i32>>, off: &Vec<Vec<Vec<i32>>>) {}

// fn overlap_check(cubes: Vec<Vec<Vec<i32>>>) -> Vec<(Vec<Vec<i32>>, Vec<Vec<i32>>)> {
//     let mut overlap_bucket = vec![];
//     for x in 0..cubes.len() - 1 {
//         for y in x + 1..cubes.len() {
//             let a = cubes[x].clone();
//             let b = cubes[y].clone();
//             if overlap(&a[0], &b[0]) && overlap(&a[1], &b[1]) && overlap(&a[2], &b[2]) {
//                 overlap_bucket.push((a, b))
//             }
//         }
//     }

//     overlap_bucket
// }

fn overlap_space(a: &Vec<Vec<i32>>, b: &Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    vec![
        helper(a[0].clone(), b[0].clone()), // wrong
        helper(a[1].clone(), b[1].clone()), // wrong
        helper(a[2].clone(), b[2].clone()), // wrong
    ] // overlap range
}

fn overlap_check(a: Vec<Vec<i32>>, b: Vec<Vec<i32>>) -> bool {
    if overlap(&a[0], &b[0]) && overlap(&a[1], &b[1]) && overlap(&a[2], &b[2]) {
        true
    } else {
        false
    }
}

fn overlap(a: &Vec<i32>, b: &Vec<i32>) -> bool {
    if a[0] < b[1] && a[1] > b[0] {
        true
    } else {
        false
    }
}

fn overlap_range(a: Vec<Vec<i32>>, b: Vec<Vec<i32>>) -> Vec<Vec<i32>> {}

fn parse_input(input: &str) -> (&str, Vec<i32>, Vec<i32>, Vec<i32>) {
    let mut a = input.split(' ');
    let command = a.next().unwrap();

    let b = a.next().unwrap().split(',').collect::<Vec<_>>();
    let x_pair = b[0].split('=').collect::<Vec<_>>();
    let y_pair = b[1].split('=').collect::<Vec<_>>();
    let z_pair = b[2].split('=').collect::<Vec<_>>();

    let x_range = x_pair[1]
        .split("..")
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    let y_range = y_pair[1]
        .split("..")
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    let z_range = z_pair[1]
        .split("..")
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    (command, x_range, y_range, z_range)
}

fn main() {
    let input = read_file_by_line("./src/bin/day22/day22.input");
    //let input = read_file_by_line("./src/bin/day22/day22_demo.input");

    //dbg!(parse_input("on x=-20..26,y=-36..17,z=-47..7"));
    //dbg!(part1(&input));
    //dbg!(part2(&input));
    dbg!(part2_v2(&input));
}
