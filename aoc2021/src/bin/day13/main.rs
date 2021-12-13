use std::collections::HashMap;

use tools::*;

fn part1(input: &Vec<String>) -> HashMap<(i32, i32), bool> {
    let mut a = input.split(|l| *l == "".to_string());
    let dots: Vec<_> = a.next().unwrap().to_vec();
    let folds: Vec<_> = a.next().unwrap().to_vec();
    let mut x_max = 0;
    let mut y_max = 0;

    let mut points_map = dots
        .iter()
        .map(|s| {
            let mut a = s.split(",");
            let this_x = a.next().unwrap().parse().unwrap();
            let this_y = a.next().unwrap().parse().unwrap();
            x_max = i32::max(this_x, x_max);
            y_max = i32::max(this_y, y_max);
            ((this_x, this_y), true)
        })
        .collect::<HashMap<(i32, i32), bool>>();

    for f in folds {
        let mut ff = f.split(" ").last().unwrap().split("=");
        let num = match (ff.next().unwrap(), ff.next().unwrap()) {
            ("y", a) => {
                points_map = foldy(&points_map, a.parse().unwrap(), (x_max, y_max));
                points_map.iter().filter(|(_, v)| **v == true).count() as i32
            }
            ("x", a) => {
                points_map = foldx(&points_map, a.parse().unwrap(), (x_max, y_max));
                points_map.iter().filter(|(_, v)| **v == true).count() as i32
            }
            _ => 0,
        };
        println!("num: {}", num);
    }

    points_map
}

fn foldy(
    orginal: &HashMap<(i32, i32), bool>,
    y: i32,
    (max_x, max_y): (i32, i32),
) -> HashMap<(i32, i32), bool> {
    let mut table = HashMap::new();
    for yy in 0..(y as usize) {
        for xx in 0..=(max_x as usize) {
            if let Some(v) = orginal.get(&(xx as i32, yy as i32)) {
                table.insert((xx as i32, yy as i32), v.clone());
            }
        }
    }

    for yy in ((y + 1) as usize)..=(max_y as usize) {
        for xx in 0..=(max_x as usize) {
            if let Some(v) = orginal.get(&(xx as i32, yy as i32)) {
                table.insert((xx as i32, y - (yy as i32) + y), v.clone());
            }
        }
    }
    table
}

fn foldx(
    orginal: &HashMap<(i32, i32), bool>,
    x: i32,
    (max_x, max_y): (i32, i32),
) -> HashMap<(i32, i32), bool> {
    let mut table = HashMap::new();
    for xx in 0..(x as usize) {
        for yy in 0..=(max_y as usize) {
            if let Some(v) = orginal.get(&(xx as i32, yy as i32)) {
                table.insert((xx as i32, yy as i32), v.clone());
            }
        }
    }

    for xx in ((x + 1) as usize)..=(max_x as usize) {
        for yy in 0..=(max_y as usize) {
            if let Some(v) = orginal.get(&(xx as i32, yy as i32)) {
                table.insert((x - (xx as i32 - x), yy as i32), v.clone());
            }
        }
    }
    table
}

fn part2(map: &HashMap<(i32, i32), bool>) {
    let mut x_max = 0;
    let mut y_max = 0;
    for ((x, y), _) in map {
        y_max = i32::max(*y, y_max);
        x_max = i32::max(*x, x_max);
    }

    for y in 0..=y_max {
        for x in 0..=x_max {
            print!(
                "{}",
                if let Some(v) = map.get(&(x, y)) {
                    "#"
                } else {
                    "."
                }
            )
        }
        println!("")
    }
}

fn main() {
    let input = read_file_by_line("./src/bin/day13/day13.input");
    //let input = read_file_by_line("./src/bin/day13/day13_demo.input");
    let table = part1(&input);
    part2(&table);
}
