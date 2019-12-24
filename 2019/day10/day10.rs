use std::collections::HashSet;
use std::fs::File;
use std::io::{prelude::*, BufRead, BufReader, Error};

fn check_connection(p: (i32, i32), sets: &Vec<(i32, i32)>) -> u32 {
    // each line will be ax = y, so if a is same, means points in same line
    let mut gra_set = HashSet::new();
    let mut gra_set_down = HashSet::new();

    let mut left = 0;
    let mut right = 0;
    let mut up = 0;
    let mut down = 0;

    for (a, b) in sets {
        if *a != p.0 || *b != p.1 {
            if *b == p.1 {
                if *a > p.0 {
                    right = 1;
                } else {
                    left = 1;
                }
            } else if *a == p.0 {
                if *b > p.1 {
                    up = 1;
                } else {
                    down = 1;
                }
            } else {
                let aa = a - p.0;
                let bb = b - p.1;
                let gra: f32 = (aa as f32) / (bb as f32);

                if aa > 0 {
                    gra_set.insert(gra.to_string());
                } else {
                    gra_set_down.insert(gra.to_string());
                }
            }
        }
    }

    gra_set.len() as u32 + gra_set_down.len() as u32 + right + left + up + down
}

//fn make_targets(p:(i32,i32),sets:&Vec<(i32,i32)>) -> {}

fn make_matrix<B: BufRead>(b: &mut B) -> Vec<(i32, i32)> {
    let (mut row, mut col) = (0, 0);
    let mut result = vec![];

    for l in b.lines() {
        for c in l.unwrap().chars() {
            if c == '#' {
                result.push((col, row));
            }
            col += 1;
        }
        col = 0;
        row += 1;
    }
    result
}

fn day10(filepath: String) -> ((i32, i32), u32) {
    let f = File::open(filepath).unwrap();
    let mut buf_reader = BufReader::new(f);

    let a = make_matrix(&mut buf_reader);
    //dbg!(&a);
    let mut a = a
        .iter()
        .map(|x| (*x, check_connection(x.clone(), &a)))
        .collect::<Vec<((i32, i32), u32)>>();
    a.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    *a.iter().last().unwrap()
}

fn main() -> Result<(), Error> {
    // let f = File::open("./day10.input")?;
    // let mut buf_reader = BufReader::new(f);

    // let a = make_matrix(&mut buf_reader);
    //println!("{}", a.len());
    //dbg!(&a);
    //dbg!(check_connection((6, 3), &a));

    dbg!(day10("./day10.input".to_string()));
    Ok(())
}
