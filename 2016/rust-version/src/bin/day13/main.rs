use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use tools::*;

// fn if_open<const F: i32>(x: i32, y: i32) -> bool {
//     ((x + y) * (x + y) + 3 * x + y + F).count_ones() % 2 == 0
// }

fn if_open(x: i32, y: i32, f: i32) -> bool {
    ((x + y) * (x + y) + 3 * x + y + f).count_ones() % 2 == 0
}

fn generate_graph(target_x: i32, target_y: i32, fav: i32) -> Graph<(i32, i32), i32> {
    let mut cache = HashSet::new();
    let mut graph = Graph::new(GraphType::Undirected);
    for x in 0..target_x * 2 {
        for y in 0..target_y * 2 {
            if !if_open(x, y, fav) {
                continue;
            }

            cache.insert((x, y));

            for (x_offset, y_offset) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                if x + x_offset < 0 || y + y_offset < 0 {
                    continue;
                }

                if cache.contains(&(x + x_offset, y + y_offset)) {
                    continue;
                }

                if if_open(x + x_offset, y + y_offset, fav) {
                    graph.insert((x, y), (x + x_offset, y + y_offset), 1);
                }
            }
        }
    }
    graph
}

fn part1(from: (i32, i32), target: (i32, i32), fav: i32) -> i32 {
    let g = generate_graph(target.0, target.1, fav);

    //dbg!(&g.get(&(6, 5)));

    let mut d = Dijkstra::new();

    d.run(&g, from, target).unwrap()
}

fn walk<ID>(
    g: &Graph<ID, i32>,
    this: ID,
    max_step: i32,
    step_count: i32,
    cache: &mut HashMap<ID, i32>,
) where
    ID: Hash + Eq + Clone,
{
    if step_count == max_step {
        return;
    }

    for next in g.get(&this).unwrap() {
        if cache.contains_key(next.id()) && *cache.get(next.id()).unwrap() <= step_count {
            continue;
        }
        cache.insert(next.id().clone(), step_count + 1);
        walk::<_>(g, next.id().clone(), max_step, step_count + 1, cache)
    }
}

fn part2(start: (i32, i32), max_step: i32, fav: i32) -> usize {
    let g = generate_graph(50, 50, fav);
    let mut cache = HashMap::new();

    cache.insert(start, 0);
    walk(&g, start, max_step, 0, &mut cache);
    //dbg!(&cache);
    cache.len()
}

fn main() {
    //dbg!(if_open::<10>(0, 0));
    //dbg!(if_open(1, 0, 10));
    //dbg!(if_open::<10>(2, 0));
    //dbg!(if_open::<10>(7, 0));
    //dbg!(if_open(5, 4, 10));

    //dbg!(part1((1, 1), (7, 4), 10));

    dbg!(part1((1, 1), (31, 39), 1352));

    //dbg!(part2((1, 1), 11, 10));
    dbg!(part2((1, 1), 50, 1352));
}
