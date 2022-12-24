use std::{
    collections::{HashMap, HashSet},
    iter::Cycle,
    slice::Iter,
    vec::IntoIter,
};

use itertools::Itertools;
use tools::*;

#[derive(Clone, Debug)]
enum Prop {
    North,
    South,
    West,
    East,
}

#[derive(Clone, Debug)]
struct Elf {
    r: i64,
    c: i64,
    going_to: Option<(i64, i64)>,
    proposes: Cycle<IntoIter<Prop>>,
}

fn parse(inputs: &Vec<String>) -> HashMap<(i64, i64), Elf> {
    //let map: Map<_> = vec![vec!["."; inputs[0].len()]; inputs.len()].into();
    let mut map = HashMap::new();
    let props = vec![Prop::North, Prop::South, Prop::West, Prop::East];

    for (r, line) in inputs.iter().enumerate() {
        for (c, cha) in line.chars().enumerate() {
            if cha == '#' {
                let aa = props.clone().into_iter().cycle();
                map.insert(
                    (r as i64, c as i64),
                    Elf {
                        r: r as i64,
                        c: c as i64,
                        going_to: None,
                        proposes: aa,
                    },
                );
            }
        }
    }

    map
}

fn end_status(map: &HashMap<(i64, i64), Elf>) -> bool {
    for ((r, c), _) in map {
        if [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ]
        .iter()
        .any(|(ro, co)| map.contains_key(&(*r + *ro, *c + *co)))
        {
            return true;
        }
    }
    false
}

fn run(map: &mut HashMap<(i64, i64), Elf>) {
    let copy_map = map.clone();
    //let mut target_set = HashSet::new();
    for ((r, c), elf) in &mut *map {
        // check around
        if [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ]
        .iter()
        .any(|(ro, co)| copy_map.contains_key(&(*r + *ro, *c + *co)))
        {
            //println!("this elf: {:?}", elf);
            let props_copy = elf.proposes.clone(); // infinity, dead loop?
            for p in props_copy.take(4) {
                match p {
                    Prop::North => {
                        if [(-1, -1), (-1, 0), (-1, 1)]
                            .iter()
                            .all(|(ro, co)| !copy_map.contains_key(&(*r + *ro, *c + *co)))
                        {
                            elf.going_to = Some((*r - 1, *c));
                            break;
                        }
                    }
                    Prop::South => {
                        if [(1, -1), (1, 0), (1, 1)]
                            .iter()
                            .all(|(ro, co)| !copy_map.contains_key(&(*r + *ro, *c + *co)))
                        {
                            elf.going_to = Some((*r + 1, *c));
                            break;
                        }
                    }
                    Prop::West => {
                        if [(1, -1), (0, -1), (-1, -1)]
                            .iter()
                            .all(|(ro, co)| !copy_map.contains_key(&(*r + *ro, *c + *co)))
                        {
                            elf.going_to = Some((*r, *c - 1));
                            break;
                        }
                    }
                    Prop::East => {
                        if [(1, 1), (0, 1), (-1, 1)]
                            .iter()
                            .all(|(ro, co)| !copy_map.contains_key(&(*r + *ro, *c + *co)))
                        {
                            elf.going_to = Some((*r, *c + 1));
                            break;
                        }
                    }
                }
            }
        }
        elf.proposes.next();
    }

    // check the target are fine
    let mut bucket = HashSet::new();
    let mut dup_ind = HashSet::new();

    for ((_r, _c), elf) in &mut *map {
        //println!("{:?}", elf);
        match elf.going_to {
            Some(target) => {
                if bucket.contains(&target) {
                    dup_ind.insert(target);
                } else {
                    bucket.insert(target);
                }
            }
            None => (),
        }
    }

    // truly move
    let mut new_map = HashMap::new();
    for ((_r, _c), elf) in &mut *map {
        match elf.going_to {
            Some(target) => {
                if !dup_ind.contains(&target) {
                    //println!("{}, {} move to {},{}", elf.r, elf.c, target.0, target.1);
                    elf.r = target.0;
                    elf.c = target.1;
                }
            }
            None => (),
        }
        new_map.insert(
            (elf.r, elf.c),
            Elf {
                r: elf.r,
                c: elf.c,
                going_to: None,
                proposes: elf.proposes.clone(),
            },
        );
    }

    *map = new_map
}

fn print_map(map: &HashMap<(i64, i64), Elf>) {
    let mut count = 0;
    for r in map.keys().map(|k| k.0).min().unwrap()..=map.keys().map(|k| k.0).max().unwrap() {
        for c in map.keys().map(|k| k.1).min().unwrap()..=map.keys().map(|k| k.1).max().unwrap() {
            if map.contains_key(&(r, c)) {
                print!("#");
            } else {
                print!(".");
                count += 1;
            }
        }
        println!("");
    }
    println!("count: {}", count);
}

fn day23(inputs: &Vec<String>, times: usize) {
    let mut map = parse(inputs);
    for _ in 0..times {
        run(&mut map);
        //print_map(&map);
    }
    print_map(&map);
}

fn day23_part2(inputs: &Vec<String>) {
    let mut map = parse(inputs);
    let mut count = 0;
    while end_status(&map) {
        run(&mut map);
        count += 1;
    }
    println!("count: {}", count + 1);
}

fn main() {
    //let input = read_file_by_line("./inputs/day23_demo.input");
    let input = read_file_by_line("./inputs/day23.input");
    day23(&input, 10);
    day23_part2(&input);
    //println!("{}", day23(&input));
    //println!("{}", day23_part2(&input));
}
