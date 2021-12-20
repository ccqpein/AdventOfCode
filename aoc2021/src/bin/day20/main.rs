use std::{collections::HashMap, str::Chars};

use tools::*;

fn part1(input: &Vec<String>, times: usize) -> usize {
    let (enhance_str, mut map) = parse_input(input);

    let mut flag = '.';

    for _ in 0..times {
        let (left_upper, right_downer) = get_edge(&map);
        //println!("edges: {:?}, {:?}", left_upper, right_downer);

        let mut new_map = HashMap::new();
        for x in left_upper.0 - 1..=right_downer.0 + 1 {
            for y in left_upper.1 - 1..=right_downer.1 + 1 {
                let all_neighbour = [
                    (x - 1, y - 1),
                    (x - 1, y),
                    (x - 1, y + 1),
                    (x, y - 1),
                    (x, y),
                    (x, y + 1),
                    (x + 1, y - 1),
                    (x + 1, y),
                    (x + 1, y + 1),
                ]
                .into_iter()
                .map(|coop| {
                    if coop.0 < left_upper.0
                        || coop.0 > right_downer.0
                        || coop.1 < left_upper.1
                        || coop.1 > right_downer.1
                    {
                        flag
                    } else {
                        *map.get(&coop).unwrap_or(&'.')
                    }
                });

                let nth = to_nth(all_neighbour.clone());

                if enhance_str[nth] == '#' {
                    new_map.insert((x, y), '#');
                }
            }
        }
        flag = if flag == '#' {
            enhance_str[511]
        } else {
            enhance_str[0]
        };

        map = new_map;
    }

    map.len()
}

fn print_map(map: &HashMap<(i32, i32), char>) {
    let ((x1, y1), (x2, y2)) = get_edge(map);
    for x in x1..=x2 {
        for y in y1..=y2 {
            print!("{}", map.get(&(x, y)).unwrap_or(&'.'))
        }
        println!()
    }
    println!()
}

fn parse_input(input: &Vec<String>) -> (Vec<char>, HashMap<(i32, i32), char>) {
    let mut input = input.iter();
    let a = input.next().unwrap().chars().collect::<Vec<char>>();
    input.next();

    let mut x = 0;
    let mut y = 0;
    let mut image = HashMap::new();
    for line in input {
        y = 0;
        for c in line.chars() {
            if c == '#' {
                image.insert((x, y), c);
            }
            y += 1;
        }
        x += 1;
    }

    (a, image)
}

fn to_nth(pixels: impl Iterator<Item = char>) -> usize {
    usize::from_str_radix(
        &String::from_iter(pixels.map(|c| if c == '.' { '0' } else { '1' })),
        2,
    )
    .unwrap()
}

fn get_edge(map: &HashMap<(i32, i32), char>) -> ((i32, i32), (i32, i32)) {
    let all_points = map.keys().collect::<Vec<&(i32, i32)>>();
    let x1 = all_points.iter().map(|(x, _)| *x).min().unwrap();

    let x2 = all_points.iter().map(|(x, _)| *x).max().unwrap();

    let y1 = all_points.iter().map(|(_, y)| *y).min().unwrap();

    let y2 = all_points.iter().map(|(_, y)| *y).max().unwrap();

    ((x1, y1), (x2, y2))
}

fn main() {
    let input = read_file_by_line("./src/bin/day20/day20.input");
    //let input = read_file_by_line("./src/bin/day20/day20_demo.input");

    println!("part1: {}", part1(&input, 2));
    println!("part2: {}", part1(&input, 50));
}
