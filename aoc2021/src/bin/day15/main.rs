use std::{
    collections::{BinaryHeap, HashMap, HashSet},
    time,
};

use tools::*;

fn part1(input: &Vec<String>) -> i32 {
    let input = input
        .iter()
        .map(|l| {
            l.chars()
                .map(|c| c.to_string().parse::<i32>().unwrap())
                .collect()
        })
        .collect::<Vec<Vec<i32>>>();

    let map = Map::from(input.clone());

    let end = (input.len() - 1, input[0].len() - 1);

    helper_v3(&map, (0, 0), end)
}

// fn helper(
//     m: &Map<i32>,
//     record: HashSet<(usize, usize)>,
//     (x, y): (usize, usize),
//     end: (usize, usize),
// ) -> Option<i32> {
//     if x == end.0 && y == end.1 {
//         return Some(m.get(end).unwrap());
//     }

//     let mut result = vec![];
//     for ((i, j), v) in m.get_around((x, y)) {
//         if record.contains(&(i, j)) {
//             continue;
//         }
//         let mut new_record = record.clone();
//         new_record.insert((i, j));
//         if let Some(next_v) = helper(m, new_record, (i, j), end) {
//             result.push(v + next_v);
//         }
//     }

//     result.iter().min().cloned()
// }

/*
fn helper_v2(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) {
    let mut record: HashMap<((usize, usize), (usize, usize)), i32> = HashMap::new();
    let mut this = start;
    //let mut stack = m.get_around(this);
    //stack.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap());
    m.get_around(this).into_iter().for_each(|(coorp, v)| {
        record.insert((start, coorp), v);
    });
    let mut already = HashSet::new();
    let mut next_start: Vec<((usize, usize), i32)> = vec![];
    loop {
        // let mut all_so_far: Vec<i32> = record.values().cloned().collect();
        // all_so_far.sort();
        // let next_start = record
        //     .iter()
        //     .filter(|(_, v)| **v == all_so_far[0])
        //     .map(|(coorp, v)| (coorp.clone(), v.clone()))
        //     .collect::<Vec<_>>();

        let mut new_next_start = vec![];
        next_start.iter().for_each(|(tail, v)| {
            let next_round = m.get_around(*tail);
            new_next_start.append(&mut next_round.clone());
            next_round
                .into_iter()
                .for_each(|(coorp, new_v)| match record.get(&(start, coorp)) {
                    Some(old_v) => {
                        if *old_v >= v + new_v {
                            record.insert((start, coorp), v + new_v);
                        }
                    }
                    None => {
                        record.insert((start, coorp), v + new_v);
                    }
                })
        });

        next_start = new_next_start;
    }
}*/

fn helper_v3(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) -> i32 {
    let mut record = HashMap::new();
    record.insert(start, 0);
    let mut already = HashSet::new();
    let mut this = start;
    let mut cache = vec![];
    loop {
        if let Some(v) = record.get(&end) {
            return *v;
        }
        let this_v = record.get(&this).unwrap().clone();
        let next_round = m.get_around_horiz(this);

        for p in next_round {
            if !already.contains(&p.0) {
                match record.get(&p.0) {
                    Some(old_v) => {
                        if *old_v >= this_v + p.1 {
                            record.insert(p.0, this_v + p.1);
                            cache.push((p.0, this_v + p.1))
                        } else {
                            cache.push((p.0, *old_v))
                        }
                    }
                    None => {
                        record.insert(p.0, this_v + p.1);
                        cache.push((p.0, this_v + p.1))
                    }
                }
            }
        }

        already.insert(this);

        // sort cache
        cache.sort_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap());
        cache = cache
            .into_iter()
            .filter(|(coorp, _)| !already.contains(coorp))
            .collect();

        this = if let Some(aa) = cache.get(0) {
            aa.0
        } else {
            break;
        };
    }
    *record.get(&end).unwrap()
}

fn part2(input: &Vec<String>) -> i32 {
    let mut cache = vec![];
    for l in input {
        cache.push(dup_line(
            l.chars()
                .map(|c| c.to_string().parse::<i32>().unwrap())
                .collect(),
        ))
    }

    cache = dup_block(cache);

    let map = Map::from(cache.clone());

    let end = (cache.len() - 1, cache[0].len() - 1);

    //dbg!(end);

    //helper_v3(&map, (0, 0), end) // about 120s

    //helper_v4(&map, (0, 0), end) // about 1.3s

    //helper_v5(&map, (0, 0), end) // about 17s after optimize

    //helper_v6(&cache) // about 12s

    helper_v7(&map, (0, 0), end) // about 4s
}

fn dup_line(mut l: Vec<i32>) -> Vec<i32> {
    let mut a: Vec<i32> = l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    let mut b: Vec<i32> = a.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    let mut c: Vec<i32> = b.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    let mut d: Vec<i32> = c.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect();
    l.append(&mut a);
    l.append(&mut b);
    l.append(&mut c);
    l.append(&mut d);
    l
}

fn dup_block(mut ll: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
    let mut a: Vec<Vec<i32>> = ll
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();
    let mut b: Vec<Vec<i32>> = a
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();
    let mut c: Vec<Vec<i32>> = b
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();
    let mut d: Vec<Vec<i32>> = c
        .iter()
        .map(|l| l.iter().map(|v| if *v == 9 { 1 } else { v + 1 }).collect())
        .collect();

    ll.append(&mut a);
    ll.append(&mut b);
    ll.append(&mut c);
    ll.append(&mut d);
    ll
}

fn helper_v4(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) -> i32 {
    let mut pg = BinaryHeap::new();
    let mut already = HashSet::new();
    already.insert(start);
    pg.push((0, start));

    let mut step = 0;
    while let Some((risk, (x, y))) = pg.pop() {
        step += 1;
        if end == (x, y) {
            println!("use {} steps", step);
            return -risk;
        }
        for ((nx, ny), v) in m.get_around_horiz((x, y)) {
            if !already.contains(&(nx, ny)) {
                already.insert((nx, ny));
                pg.push((risk - v, (nx, ny)));
            }
        }
    }
    0
}

fn helper_v5(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) -> i32 {
    let time_stamp = time::Instant::now();
    let mut done = false;

    let (x, y) = (end.0 + 1, end.1 + 1);
    let mut dists = vec![vec![i32::MAX; y]; x];

    dists[0][0] = 0;

    println!("after init: {}", time_stamp.elapsed().as_micros());

    let mut step = 0;
    while !done {
        step += 1;
        done = true;

        for r in 0..=end.0 {
            for c in 0..=end.1 {
                let current_risk = dists[r][c];
                for ((nx, ny), v) in m.get_around_horiz((r, c)) {
                    if dists[nx][ny] > current_risk + v {
                        done = false;
                        dists[nx][ny] = current_risk + v
                    }
                }
            }
        }
        println!("after step {}: {}", step, time_stamp.elapsed().as_micros());
    }
    println!("done: {}", time_stamp.elapsed().as_micros());
    println!("use {} steps", step);
    dists[end.0][end.1]
}

const NEIGHBORS: [(i32, i32); 4] = [(1, 0), (-1, 0), (0, 1), (0, -1)];

fn helper_v6(grid: &Vec<Vec<i32>>) -> i32 {
    let time_stamp = time::Instant::now();
    let m = grid.len();
    let n = grid[0].len();

    let mut dists = vec![vec![i32::MAX; n]; m];

    dists[0][0] = 0;
    println!("after init: {}", time_stamp.elapsed().as_micros());
    let m = m as i32;
    let n = n as i32;
    let mut done = false;

    let mut loops = 0;
    while !done {
        loops += 1;
        done = true;
        for i in 0..m {
            for j in 0..n {
                let current_risk = dists[i as usize][j as usize];

                let (row, col) = (i as i32, j as i32);
                for (dr, dc) in NEIGHBORS.iter() {
                    let (newr, newc) = (row + dr, col + dc);
                    if newr >= 0 && newr < m && newc >= 0 && newc < n {
                        if dists[newr as usize][newc as usize]
                            > current_risk + grid[newr as usize][newc as usize]
                        {
                            done = false;
                            dists[newr as usize][newc as usize] =
                                current_risk + grid[newr as usize][newc as usize];
                        }
                    }
                }
            }
        }
        println!("after step {}: {}", loops, time_stamp.elapsed().as_micros());
    }

    println!("loops: {}", loops);
    dists[m as usize - 1][n as usize - 1]
}

fn helper_v7(m: &Map<i32>, start: (usize, usize), end: (usize, usize)) -> i32 {
    let mut g = Graph::new();
    for (id, _) in m {
        for (x, v) in m.get_around_horiz(id) {
            g.insert(id, x, v)
        }
    }

    let mut dj = Dijkstra::new();
    dj.run(&g, start, end)
}

fn main() {
    let input = read_file_by_line("./src/bin/day15/day15.input");
    //let input = read_file_by_line("./src/bin/day15/day15_demo.input");
    //println!("{:?}", part1(&input));
    println!("{:?}", part2(&input));
}
