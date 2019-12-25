use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Error};

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

fn make_targets(p: (i32, i32), sets: &Vec<(i32, i32)>) -> Vec<(i32, i32)> {
    let mut right: HashMap<String, Vec<(i32, i32)>> = HashMap::new();
    let mut left: HashMap<String, Vec<(i32, i32)>> = HashMap::new();

    let mut up = vec![];
    let mut down = vec![];

    for (a, b) in sets {
        if *a != p.0 || *b != p.1 {
            if *a == p.0 {
                if *b > p.1 {
                    //left.entry("ver".to_string()).or_insert(vec![]) // 12 o'clock
                    down.push((*a, *b));
                } else {
                    //right.entry("ver".to_string()).or_insert(vec![]) // 6 o'clock
                    up.push((*a, *b));
                };

                continue;
            }

            if *b == p.1 {
                let temp = if *a > p.0 {
                    right.entry("0".to_string()).or_insert(vec![]) // 3 o'clock
                } else {
                    left.entry("0".to_string()).or_insert(vec![]) // 9 o'clock
                };

                temp.push((*a, *b));
                continue;
            }

            let aa = a - p.0;
            let bb = p.1 - b;
            let gra: f32 = (bb as f32) / (aa as f32);

            if aa > 0 {
                let temp = right.entry(gra.to_string()).or_insert(vec![]);
                temp.push((*a, *b));
            } else {
                let temp = left.entry(gra.to_string()).or_insert(vec![]);
                temp.push((*a, *b));
            }
        }
    }

    up.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap()); // sort up
    down.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap()); // sort down
    for (_k, v) in &mut right {
        // sort right
        v.sort_by(|a, b| {
            let dis1 = a.0 - p.0 + a.1 - p.1;
            let dis2 = b.0 - p.0 + b.1 - p.1;
            dis2.partial_cmp(&dis1).unwrap()
        })
    }

    for (_k, v) in &mut left {
        // sort left
        v.sort_by(|a, b| {
            let dis1 = a.0 - p.0 + a.1 - p.1;
            let dis2 = b.0 - p.0 + b.1 - p.1;
            dis1.partial_cmp(&dis2).unwrap()
        })
    }

    //println!("{:?}", up);
    //println!("{:?}", down);
    //println!("{:?}", right);
    //println!("{:?}", left);
    attack(right, left, up, down)
}

type S = HashMap<String, Vec<(i32, i32)>>;

fn attack(
    mut right: S,
    mut left: S,
    mut up: Vec<(i32, i32)>,
    mut down: Vec<(i32, i32)>,
) -> Vec<(i32, i32)> {
    let mut result: Vec<(i32, i32)> = vec![];

    while up.len() != 0 || down.len() != 0 || right.len() != 0 || left.len() != 0 {
        if up.len() != 0 {
            //println!("12 o'clock -> {:?}", up.first().unwrap());
            result.push(up.first().unwrap().clone());
            up.drain(..1);
        }

        let mut keys = right.keys().map(|k| k.clone()).collect::<Vec<String>>();
        keys.sort_by(|a, b| {
            b.parse::<f32>()
                .unwrap()
                .partial_cmp(&a.parse::<f32>().unwrap())
                .unwrap()
        });
        for k in keys {
            let this_ele = right.get_mut(&k).unwrap().pop().unwrap().clone();
            //println!("right -> {} -> {:?}", k, &this_ele);
            result.push(this_ele);
            if right.get(&k).unwrap().len() == 0 {
                right.remove(&k);
            }
        }

        if down.len() != 0 {
            //println!("6 o'clock -> {:?}", down.first().unwrap());
            result.push(down.first().unwrap().clone());
            down.drain(..1);
        }

        let mut keys = left.keys().map(|k| k.clone()).collect::<Vec<String>>();
        keys.sort_by(|a, b| {
            b.parse::<f32>()
                .unwrap()
                .partial_cmp(&a.parse::<f32>().unwrap())
                .unwrap()
        });
        for k in keys {
            let this_ele = left.get_mut(&k).unwrap().pop().unwrap().clone();
            //println!("left -> {} -> {:?}", k, &this_ele);
            result.push(this_ele);
            if left.get(&k).unwrap().len() == 0 {
                left.remove(&k);
            }
        }
    }

    result
}

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

fn day10_part2(filepath: String, p: (i32, i32), num: usize) -> (i32, i32) {
    let f = File::open(filepath).unwrap();
    let mut buf_reader = BufReader::new(f);

    let a = make_matrix(&mut buf_reader);

    make_targets(p, &a)[num]
}

fn main() -> Result<(), Error> {
    // let f = File::open("./day10.input")?;
    // let mut buf_reader = BufReader::new(f);

    // let a = make_matrix(&mut buf_reader);
    // println!("{:?}", make_targets((11, 13), &a)[198]);

    //dbg!(day10("./day10.input".to_string()));

    dbg!(day10_part2("./day10.input".to_string(), (23, 29), 199));
    Ok(())
}
