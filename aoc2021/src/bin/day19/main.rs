#![feature(destructuring_assignment)]
use std::collections::{HashMap, HashSet};

use tools::*;

fn day19(
    input: &Vec<String>,
) -> (
    HashMap<usize, usize>,
    HashMap<usize, (i32, i32, i32)>,
    HashSet<(i32, i32, i32)>,
) {
    let mut all_points: Vec<Vec<(i32, i32, i32)>> = vec![];
    let mut cache = vec![];
    for l in &input.as_slice()[1..] {
        if l == "" {
            //l.split(' ').nth(2).unwrap().parse::<usize>();
            all_points.push(cache.clone());
            cache.clear()
        } else if l.starts_with("--- ") {
        } else {
            cache.push(line_parse(l))
        }
    }
    all_points.push(cache);
    // each chunks all 24 posible options
    let mut all_points_options = all_points
        .iter()
        .map(|chunk| zip_all(chunk.to_vec()))
        .collect::<Vec<Vec<Vec<(i32, i32, i32)>>>>();

    let mut rr = HashMap::new();
    rr.insert(0, 0);

    let mut oo = HashMap::new();
    oo.insert(0, (0, 0, 0));

    let mut nt: HashMap<usize, HashSet<usize>> = HashMap::new(); // default value is hashset
    let mut b = all_points_options[0][0]
        .iter()
        .cloned()
        .collect::<HashSet<_>>();

    while rr.len() < all_points_options.len() {
        'aaa: for (i, chunk) in all_points_options.iter().enumerate() {
            if rr.contains_key(&i) {
                continue;
            }
            let mut temp_k = 0_usize;
            //let mut add_flag = false;
            for (k, v) in &rr {
                if nt.entry(i).or_insert(HashSet::new()).contains(&k) {
                    continue;
                }
                let scj = all_points_options[*k][*v as usize].clone();
                let (axx, ayy, azz) = oo.get(k).unwrap().clone();

                for rot_idx in 0..24 {
                    if let Some((ox, oy, oz)) = check(&scj, &chunk[rot_idx]) {
                        //println!("{:?}", (ox, oy, oz));
                        let new_s = chunk[rot_idx]
                            .iter()
                            .map(|(x, y, z)| (x - ox - axx, y - oy - ayy, z - oz - azz))
                            .collect::<HashSet<_>>();
                        b = b.union(&new_s).cloned().collect();

                        rr.insert(i, rot_idx);
                        oo.insert(i, (ox + axx, oy + ayy, oz + azz));
                        break 'aaa;
                    } else {
                        continue;
                    }
                }
                temp_k = *k;
            }

            nt.entry(i).or_insert(HashSet::new()).insert(temp_k);
        }
    }

    return (rr, oo, b);
}

fn zip_all(chunk: Vec<(i32, i32, i32)>) -> Vec<Vec<(i32, i32, i32)>> {
    let all_options = chunk
        .into_iter()
        .map(|line| all_24_options(line))
        .collect::<Vec<Vec<(i32, i32, i32)>>>();

    (0..24)
        .into_iter()
        .map(|i| {
            all_options
                .iter()
                .map(|l| l[i])
                .collect::<Vec<(i32, i32, i32)>>()
        })
        .collect()
}

fn check(aa: &Vec<(i32, i32, i32)>, bb: &Vec<(i32, i32, i32)>) -> Option<(i32, i32, i32)> {
    let set: HashSet<(i32, i32, i32)> = aa.iter().cloned().collect();
    for (a, b, c) in aa {
        for (d, e, f) in bb {
            let set_match = bb
                .iter()
                .map(|(x, y, z)| (x - d + a, y - e + b, z - f + c))
                .collect::<HashSet<(i32, i32, i32)>>();

            if set.intersection(&set_match).count() == 12 {
                return Some((d - a, e - b, f - c));
            }
        }
    }

    None
}

fn all_24_options((mut a, mut b, mut c): (i32, i32, i32)) -> Vec<(i32, i32, i32)> {
    let r = |(a, b, c): (i32, i32, i32)| (a, c, -b);
    let t = |(a, b, c): (i32, i32, i32)| (-b, a, c);

    let mut result = vec![];

    for _ in 0..2 {
        for _ in 0..3 {
            (a, b, c) = r((a, b, c));
            result.push((a, b, c));
            for _ in 0..3 {
                (a, b, c) = t((a, b, c));
                result.push((a, b, c));
            }
        }
        (a, b, c) = r(t(r((a, b, c))));
    }
    result
}

fn line_parse(l: &str) -> (i32, i32, i32) {
    let mut a = l.split(',').map(|w| w.parse::<i32>().unwrap());
    (a.next().unwrap(), a.next().unwrap(), a.next().unwrap())
}

fn main() {
    let input = read_file_by_line("./src/bin/day19/day19.input");
    //let input = read_file_by_line("./src/bin/day19/day19_demo.input");
    let (rr, oo, b) = day19(&input);
    println!("part1: {:?}", b.len());
    let mut distance = 0;
    for i in 0..oo.len() {
        let (x, y, z) = oo.get(&i).unwrap();
        for j in 0..oo.len() {
            let (xx, yy, zz) = oo.get(&j).unwrap();
            let ddd = (xx - x).abs() + (yy - y).abs() + (zz - z).abs();
            if ddd > distance {
                distance = ddd;
            }
        }
    }
    println!("part2: {:?}", distance);
}
