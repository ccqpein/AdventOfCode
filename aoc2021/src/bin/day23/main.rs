use std::collections::HashMap;

use tools::*;

// fn energy_take(x: char) -> usize {
//     match x {
//         'A' => 1,
//         'B' => 10,
//         'C' => 100,
//         'D' => 1000,
//         _ => unreachable!(),
//     }
// }

// #[derive(Clone)]
// struct Hallway {
//     inner: [Option<char>; 11],
// }

// impl Hallway {
//     fn add_from_hrd(&self, c: char, idx: usize) -> Option<Vec<Self>> {
//         let mut all_possible_loc = vec![];
//         let mut i = idx;
//         loop {
//             i -= 1;
//             if i == 2 || i == 4 || i == 6 || i == 8 {
//                 continue;
//             }
//             if self.inner[i].is_none() {
//                 all_possible_loc.push(i)
//             }
//             if i == 0 {
//                 break;
//             }
//         }
//         let mut i = idx;
//         loop {
//             i += 1;
//             if i == 2 || i == 4 || i == 6 || i == 8 {
//                 continue;
//             }
//             if self.inner[i].is_none() {
//                 all_possible_loc.push(i)
//             }
//             if i == 10 {
//                 break;
//             }
//         }

//         if all_possible_loc.is_empty() {
//             None
//         } else {
//             Some(
//                 all_possible_loc
//                     .iter()
//                     .map(|i| {
//                         let mut temp = self.clone();
//                         temp.inner[*i] = Some(c);
//                         temp
//                     })
//                     .collect(),
//             )
//         }
//     }

//     fn remove_to_hrd(&self, hrd: &HRD) {}
// }

// #[derive(Clone)]
// struct HRD {
//     up: [Option<char>; 4],
//     down: [Option<char>; 4],
// }

// impl HRD {
//     fn is_end(&self) -> bool {
//         match (self.up, self.down) {
//             (
//                 [Some('A'), Some('B'), Some('C'), Some('D')],
//                 [Some('A'), Some('B'), Some('C'), Some('D')],
//             ) => true,
//             _ => false,
//         }
//     }

//     fn is_dead(&self) -> bool {
//         !self.is_end()
//             && self.up.iter().all(|e| e.is_some())
//             && self.down.iter().all(|e| e.is_some())
//     }

//     fn copy_and_remove(&self, up: bool, idx: usize) -> Self {
//         let mut result = self.clone();
//         if up {
//             result.up[idx] = None;
//         } else {
//             result.down[idx] = None;
//         }

//         result
//     }

//     fn pick_one(&self) -> Vec<(char, usize, Self)> {
//         let mut result = vec![];
//         for (idx, c) in self.up.iter().enumerate() {
//             if let Some(cc) = c {
//                 let hallway_idx = match idx {
//                     0 => 2,
//                     1 => 4,
//                     2 => 6,
//                     3 => 8,
//                     _ => unreachable!(),
//                 };
//                 result.push((*cc, hallway_idx, self.copy_and_remove(true, idx)))
//             } else {
//                 if let Some(cc) = self.down[idx] {
//                     let hallway_idx = match idx {
//                         0 => 2,
//                         1 => 4,
//                         2 => 6,
//                         3 => 8,
//                         _ => unreachable!(),
//                     };
//                     result.push((cc, hallway_idx, self.copy_and_remove(false, idx)))
//                 }
//             }
//         }
//         result
//     }

//     fn all_slots(&self) -> Vec<usize> {}
// }

// fn move_one_step(hw: Hallway, hrd: HRD) {}

//=======================================================

// const WEIGHT: [i64; 4] = [1, 10, 100, 1000];
// const BUF_WEIGHT: [i64; 7] = [0, 1, 3, 5, 7, 9, 10];

// fn weight(c: char) -> i64 {
//     WEIGHT[(c as u8 - 'A' as u8) as usize]
// }

// fn buf_traversal_cost(i: usize, j: usize, c: char) -> i64 {
//     (BUF_WEIGHT[i] - BUF_WEIGHT[j]).abs() * WEIGHT[(c as u8 - 'A' as u8) as usize]
// }

// #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// struct State {
//     r: [Vec<char>; 4],
//     b: [char; 7],
// }

// impl State {
//     fn is_valid_room(&self, i: usize) -> bool {
//         self.r[i]
//             .iter()
//             .all(|&c| i == (c as u8 - 'A' as u8) as usize)
//     }

//     fn entry_cost(&self, i: usize) -> i64 {
//         (4 - self.r[i].len()) as i64 * WEIGHT[i]
//     }

//     fn exit_cost(&self, i: usize, c: char) -> i64 {
//         (4 - self.r[i].len()) as i64 * weight(c)
//     }

//     fn transition_room_to_buffer(&self) -> Vec<(State, i64)> {
//         let mut res = vec![];
//         for i in 0..4 {
//             if self.is_valid_room(i) {
//                 continue;
//             }
//             let mut next = self.clone();
//             let c = next.r[i].pop().unwrap();
//             for j in (0..=i + 1).rev() {
//                 let cost = buf_traversal_cost(j, i + 1, c) + weight(c) + next.exit_cost(i, c);
//                 if next.b[j] == '.' {
//                     next.b[j] = c;
//                     res.push((next.clone(), cost));
//                     next.b[j] = '.';
//                 } else {
//                     break;
//                 }
//             }
//             for j in i + 2..7 {
//                 let cost = buf_traversal_cost(i + 2, j, c) + weight(c) + next.exit_cost(i, c);
//                 if next.b[j] == '.' {
//                     next.b[j] = c;
//                     res.push((next.clone(), cost));
//                     next.b[j] = '.';
//                 } else {
//                     break;
//                 }
//             }
//         }
//         res
//     }

//     fn transition_buffer_to_room(&self) -> Vec<(State, i64)> {
//         let mut res = vec![];
//         for i in 0..7 {
//             if self.b[i] == '.' {
//                 continue;
//             }
//             let r = (self.b[i] as u8 - 'A' as u8) as usize;
//             if !self.is_valid_room(r) {
//                 continue;
//             }
//             if i <= r + 1 {
//                 if (i + 1..=r + 1).all(|i| self.b[i] == '.') {
//                     let mut next = self.clone();
//                     let c = buf_traversal_cost(i, r + 1, next.b[i])
//                         + weight(next.b[i])
//                         + self.entry_cost(r);
//                     next.r[r].push(next.b[i]);
//                     next.b[i] = '.';
//                     res.push((next, c));
//                 }
//             } else {
//                 if (r + 2..i).all(|i| self.b[i] == '.') {
//                     let mut next = self.clone();
//                     let c = buf_traversal_cost(r + 2, i, next.b[i])
//                         + weight(next.b[i])
//                         + self.entry_cost(r);
//                     next.r[r].push(next.b[i]);
//                     next.b[i] = '.';
//                     res.push((next, c));
//                 }
//             }
//         }
//         res
//     }

//     fn transitions(&self) -> Vec<(State, i64)> {
//         let mut res = self.transition_room_to_buffer();
//         res.append(&mut self.transition_buffer_to_room());
//         res
//     }
// }

// fn day23() {
//     let input = State {
//         r: [
//             vec!['D', 'D', 'D', 'A'],
//             vec!['D', 'B', 'C', 'C'],
//             vec!['B', 'A', 'B', 'B'],
//             vec!['C', 'C', 'A', 'A'],
//         ],
//         // r: [
//         //     vec!['D', 'A'],
//         //     vec!['D', 'C'],
//         //     vec!['B', 'B'],
//         //     vec!['C', 'A'],
//         // ],
//         b: ['.'; 7],
//     };
//     let expected = State {
//         r: [
//             vec!['A', 'A', 'A', 'A'],
//             vec!['B', 'B', 'B', 'B'],
//             vec!['C', 'C', 'C', 'C'],
//             vec!['D', 'D', 'D', 'D'],
//         ],
//         // r: [
//         //     vec!['A', 'A'],
//         //     vec!['B', 'B'],
//         //     vec!['C', 'C'],
//         //     vec!['D', 'D'],
//         // ],
//         b: ['.'; 7],
//     };

//     let mut costs = HashMap::new();
//     let mut q = std::collections::BinaryHeap::new();
//     costs.insert(input.clone(), 0);
//     q.push((0, input));
//     while let Some((cost, grid)) = q.pop() {
//         let cost = -cost;
//         if cost != costs[&grid] {
//             continue;
//         }
//         if grid == expected {
//             break;
//         }
//         for (transition, t_cost) in grid.transitions() {
//             if let Some(&c) = costs.get(&transition) {
//                 if c <= t_cost + cost {
//                     continue;
//                 }
//             }
//             costs.insert(transition.clone(), t_cost + cost);
//             q.push((-(t_cost + cost), transition));
//         }
//     }

//     println!("{:?}", costs.get(&expected));
// }

use lazy_static::*;
lazy_static! {
    static ref PATH: HashMap<(usize, usize), (usize, Vec<usize>)> = {
        let mut m = HashMap::new();
        m.insert((0, 0), (3, vec![1]));
        m.insert((0, 1), (5, vec![1, 2]));
        m.insert((0, 2), (7, vec![1, 2, 3]));
        m.insert((0, 3), (9, vec![1, 2, 3, 4]));
        m.insert((1, 0), (2, vec![]));
        m.insert((1, 1), (4, vec![2]));
        m.insert((1, 2), (6, vec![2, 3]));
        m.insert((1, 3), (8, vec![2, 4]));
        m.insert((2, 0), (2, vec![]));
        m.insert((2, 1), (2, vec![]));
        m.insert((2, 2), (4, vec![3]));
        m.insert((2, 3), (6, vec![3, 4]));
        m.insert((3, 0), (4, vec![2]));
        m.insert((3, 1), (2, vec![]));
        m.insert((3, 2), (2, vec![]));
        m.insert((3, 3), (4, vec![4]));
        m.insert((4, 0), (6, vec![2, 3]));
        m.insert((4, 1), (4, vec![3]));
        m.insert((4, 2), (2, vec![]));
        m.insert((4, 3), (2, vec![]));
        m.insert((5, 0), (8, vec![2, 3, 4]));
        m.insert((5, 1), (6, vec![3, 4]));
        m.insert((5, 2), (4, vec![4]));
        m.insert((5, 3), (2, vec![]));
        m.insert((6, 0), (9, vec![2, 3, 4, 5]));
        m.insert((6, 1), (7, vec![3, 4, 5]));
        m.insert((6, 2), (5, vec![4, 5]));
        m.insert((6, 3), (3, vec![5]));
        m
    };
}

#[derive(Clone, Debug)]
struct HRD {
    rooms: [Vec<usize>; 4],
    hallway: [usize; 7],
    score: usize,
}

impl HRD {
    fn new(rooms: [Vec<usize>; 4], hallway: [usize; 7], score: usize) -> Self {
        Self {
            rooms,
            hallway,
            score,
        }
    }

    fn hrd_move(
        &self,
        h_i: usize,
        h_v: usize,
        room_i: usize,
        room_pos: usize,
        room_v: usize,
        score: usize,
    ) -> Self {
        let mut rooms = self.rooms.clone();
        rooms[room_i][room_pos] = room_v;
        let mut hallway = self.hallway.clone();
        hallway[h_i] = h_v;
        Self::new(rooms, hallway, self.score + score)
    }
}

fn check(hrd: HRD) -> Vec<HRD> {
    let mut new_hrds = vec![];

    for (h_i, mover) in hrd.hallway.iter().enumerate() {
        if *mover != 0 {
            let room_i = mover - 1;
            if hrd.rooms[room_i]
                .iter()
                .all(|cell| *cell == 0 || cell == mover)
            {
                let (distance, cells) = PATH.get(&(h_i, room_i)).cloned().unwrap();
                if !cells.iter().any(|cell| hrd.hallway[*cell] != 0) {
                    let room_pos = hrd.rooms[room_i].iter().filter(|e| **e == 0).count() - 1;
                    let score = (distance + room_pos) * usize::pow(10, (*mover as u32 - 1));
                    new_hrds.push(hrd.hrd_move(h_i, 0, room_i, room_pos, *mover, score));
                }
            }
        }
    }

    for room_i in 0..4 {
        let room = hrd.rooms[room_i].clone();
        if room.iter().all(|&cell| cell == room_i + 1 || cell == 0) {
            continue;
        }
        let room_pos = room.iter().filter(|&e| *e == 0).count();
        let mover = room[room_pos];
        for (h_i, occupant) in hrd.hallway.iter().enumerate() {
            if *occupant == 0 {
                let (distance, cells) = PATH.get(&(h_i, room_i)).unwrap();
                if !cells.iter().any(|cell| hrd.hallway[*cell] != 0) {
                    let score = (distance + room_pos) * usize::pow(10, (mover as u32 - 1));
                    new_hrds.push(hrd.hrd_move(h_i, mover, room_i, room_pos, 0, score));
                }
            }
        }
    }
    new_hrds
}

fn done(hrd: &HRD) -> bool {
    for (room_i, cells) in hrd.rooms.iter().enumerate() {
        if !cells.iter().all(|cell| *cell == room_i + 1) {
            return false;
        }
    }
    return true;
}

fn prune(hrds: Vec<HRD>) -> Vec<HRD> {
    let mut record: HashMap<([Vec<usize>; 4], [usize; 7]), HRD> = HashMap::new();
    for hrd in hrds {
        let k = (hrd.rooms.clone(), hrd.hallway);
        if let Some(vv) = record.get_mut(&k) {
            if vv.score > hrd.score {
                vv.score = hrd.score
            }
        } else {
            record.insert(k, hrd.clone());
        }
    }
    record.into_values().collect()
}

fn day23(hrd: HRD) -> usize {
    let mut done_hrd = vec![];
    let mut hrds = vec![hrd];

    while hrds.len() != 0 {
        //println!("{}", hrds.len());
        let mut new_hrds = vec![];
        for hrd in hrds {
            for new_hrd in check(hrd) {
                //println!("here?");
                if done(&new_hrd) {
                    done_hrd.push(new_hrd)
                } else {
                    new_hrds.push(new_hrd)
                }
            }
        }
        hrds = prune(new_hrds);
    }
    //dbg!(&done_hrd);
    done_hrd.iter().map(|h| h.score).min().unwrap()
}

fn main() {
    //let input = read_file_by_line("./src/bin/day23/day23.input");
    //let input = read_file_by_line("./src/bin/day23/day23_demo.input");
    println!(
        "part1: {}",
        day23(HRD::new(
            [vec![1, 4], vec![3, 4], vec![2, 2], vec![1, 3]],
            [0; 7],
            0,
        ))
    );

    println!(
        "part2: {}",
        day23(HRD::new(
            [
                vec![1, 4, 4, 4],
                vec![3, 3, 2, 4],
                vec![2, 2, 1, 2],
                vec![1, 1, 3, 3]
            ],
            [0; 7],
            0,
        ))
    );
}
