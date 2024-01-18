// #![feature(let_chains)]

// use std::collections::HashMap;

// use tools::*;

// #[derive(Debug, Clone)]
// enum Cost {
//     Ore,      // ind 0
//     Clay,     // ind 1
//     Obsidian, // ind 2
//     Geode,    // ind 3
// }

// impl Cost {
//     fn to_ind(&self) -> usize {
//         match self {
//             Cost::Ore => 0,
//             Cost::Clay => 1,
//             Cost::Obsidian => 2,
//             Cost::Geode => 3,
//         }
//     }
// }

// impl From<&'_ str> for Cost {
//     fn from(value: &'_ str) -> Self {
//         match value {
//             "ore" => Cost::Ore,
//             "clay" => Cost::Clay,
//             "obsidian" => Cost::Obsidian,
//             "geode" => Cost::Geode,
//             _ => unreachable!(),
//         }
//     }
// }

// #[derive(Debug, Clone)]
// struct Robot {
//     get: Cost,
//     cost: Vec<(usize, Cost)>,
// }

// impl Robot {
//     // make this robot and return with the new bucket
//     fn make_from_bucket(&self, bucket: &[usize; 4]) -> Option<(Self, [usize; 4])> {
//         if self.cost.iter().all(|(n, c)| bucket[c.to_ind()] >= *n) {
//             let mut new_bucket = bucket.clone();
//             self.cost
//                 .iter()
//                 .for_each(|(n, c)| new_bucket[c.to_ind()] -= n);
//             return Some((self.clone(), new_bucket));
//         }

//         None
//     }

//     fn gain(&self, bucket: &mut [usize; 4]) {
//         bucket[self.get.to_ind()] += 1;
//     }
// }

// fn parse_input(input: &[String]) -> Vec<Vec<Robot>> {
//     let mut result = vec![];
//     for line in input {
//         if line == "" {
//             continue;
//         }

//         let mut this_blue_p = vec![];

//         let mut ss = line.split(&['.', ':'][..]);
//         ss.next();
//         for b in ss {
//             if b == "" {
//                 continue;
//             }
//             let mut this_bot = Robot {
//                 get: Cost::Ore,
//                 cost: vec![],
//             };
//             let mut bb = b.split(' ');
//             //println!("{:?}", bb.clone().collect::<Vec<_>>());
//             while let Some(w) = bb.next() {
//                 match w {
//                     "Each" => {
//                         this_bot.get = Cost::from(bb.next().unwrap());
//                     }
//                     "costs" => this_bot.cost.push((
//                         bb.next().unwrap().parse::<usize>().unwrap(),
//                         Cost::from(bb.next().unwrap()),
//                     )),
//                     "and" => this_bot.cost.push((
//                         bb.next().unwrap().parse::<usize>().unwrap(),
//                         Cost::from(bb.next().unwrap()),
//                     )),
//                     _ => {}
//                 }
//             }
//             this_blue_p.push(this_bot)
//         }

//         result.push(this_blue_p);
//     }
//     result
// }

// fn find_the_most_each_blue_p(
//     blue_p: &Vec<Robot>,
//     running_bots: Vec<Robot>,
//     bucket: [usize; 4],
//     min_left: usize,
//     record: &mut HashMap<([usize; 4], usize, [usize; 4]), usize>,
// ) -> usize {
//     if min_left == 0 {
//         return bucket[3];
//     }

//     let mut running_bots_set = [0; 4];
//     running_bots
//         .iter()
//         .for_each(|b| running_bots_set[b.get.to_ind()] += 1);

//     if let Some(rr) = record.get(&(bucket, min_left, running_bots_set)) {
//         return *rr;
//     }

//     let mut result = 0;
//     for b in blue_p {
//         if let Some((bb, mut new_bucket)) = b.make_from_bucket(&bucket) {
//             let mut new_running_bots = running_bots.clone();

//             new_running_bots.iter().for_each(|b| {
//                 b.gain(&mut new_bucket);
//             });

//             new_running_bots.push(bb);

//             let next = find_the_most_each_blue_p(
//                 blue_p,
//                 new_running_bots,
//                 new_bucket,
//                 min_left - 1,
//                 record,
//             );
//             if next > result {
//                 result = next;
//             }
//         } else {
//             let mut new_bucket = bucket.clone();
//             let new_running_bots = running_bots.clone();
//             new_running_bots.iter().for_each(|b| {
//                 b.gain(&mut new_bucket);
//             });

//             let next = find_the_most_each_blue_p(
//                 blue_p,
//                 new_running_bots,
//                 new_bucket,
//                 min_left - 1,
//                 record,
//             );
//             if next > result {
//                 result = next;
//             }
//         }
//     }

//     record.insert((bucket, min_left, running_bots_set), result);
//     //println!("min_left: {}, result: {}", min_left, result);

//     result
// }

// fn part1(blue_ps: &Vec<Vec<Robot>>) -> usize {
//     let mut result = 0;
//     for i in 0..blue_ps.len() {
//         let mut record = HashMap::new();
//         result += (1 + i)
//             * find_the_most_each_blue_p(
//                 &blue_ps[0],
//                 vec![blue_ps[0][0].clone()],
//                 [0; 4],
//                 24,
//                 &mut record,
//             )
//     }

//     result
// }

// fn main() {
//     let input = read_file_by_line("./inputs/day19_demo.input");
//     //println!("{:?}", parse_input(&input));
//     let blue_ps = parse_input(&input);
//     let mut record = HashMap::new();
//     println!(
//         "{}",
//         find_the_most_each_blue_p(
//             &blue_ps[0],
//             vec![blue_ps[0][0].clone()],
//             [0; 4],
//             24,
//             &mut record
//         )
//     );
// }

//////////////////////////////////////////////////////////
use std::fs;

#[derive(Debug)]
struct Blueprint {
    ore_robot_cost: i32,
    clay_robot_cost: i32,
    obsidian_robot_cost_ore: i32,
    obsidian_robot_cost_clay: i32,
    geode_robot_cost_ore: i32,
    geode_robot_cost_obsidian: i32,

    max_ore_use_per_turn: i32,
}

#[derive(PartialEq, Clone, Eq, Hash)]
enum Construction {
    OreRobot,
    ClayRobot,
    ObsidianRobot,
    GeodeRobot,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct State {
    ore: i32,
    clay: i32,
    obsidian: i32,
    geodes: i32,

    ore_robots: i32,
    clay_robots: i32,
    obsidian_robots: i32,
    geode_robots: i32,

    construction: Construction,
}

fn find_most_geodes(blueprint: &Blueprint, mut remaining_turns: i32, mut state: State) -> i32 {
    let mut robot_constructed = false;
    while robot_constructed == false && remaining_turns > 0 {
        match state.construction {
            Construction::OreRobot => {
                if state.ore >= blueprint.ore_robot_cost {
                    state.ore -= blueprint.ore_robot_cost;
                    robot_constructed = true;
                }
            }
            Construction::ClayRobot => {
                if state.ore >= blueprint.clay_robot_cost {
                    state.ore -= blueprint.clay_robot_cost;
                    robot_constructed = true;
                }
            }
            Construction::ObsidianRobot => {
                if state.ore >= blueprint.obsidian_robot_cost_ore
                    && state.clay >= blueprint.obsidian_robot_cost_clay
                {
                    state.ore -= blueprint.obsidian_robot_cost_ore;
                    state.clay -= blueprint.obsidian_robot_cost_clay;
                    robot_constructed = true;
                }
            }
            Construction::GeodeRobot => {
                if state.ore >= blueprint.geode_robot_cost_ore
                    && state.obsidian >= blueprint.geode_robot_cost_obsidian
                {
                    state.ore -= blueprint.geode_robot_cost_ore;
                    state.obsidian -= blueprint.geode_robot_cost_obsidian;
                    robot_constructed = true;
                }
            }
        }
        state.ore += state.ore_robots;
        state.clay += state.clay_robots;
        state.obsidian += state.obsidian_robots;
        state.geodes += state.geode_robots;
        remaining_turns -= 1;
        if robot_constructed {
            match state.construction {
                Construction::OreRobot => state.ore_robots += 1,
                Construction::ClayRobot => state.clay_robots += 1,
                Construction::ObsidianRobot => state.obsidian_robots += 1,
                Construction::GeodeRobot => state.geode_robots += 1,
            }
        }
    }

    // Decide what do build next. Always possible to build ore and clay robots but
    let mut max_geodes = state.geodes;
    if remaining_turns > 0 {
        for next_robot in [
            Construction::OreRobot,
            Construction::ClayRobot,
            Construction::ObsidianRobot,
            Construction::GeodeRobot,
        ] {
            // Dont evaluate paths that have unbuildablle robots
            if next_robot == Construction::ObsidianRobot && state.clay_robots == 0 {
                continue;
            }
            if next_robot == Construction::GeodeRobot && state.obsidian_robots == 0 {
                continue;
            }
            // Dont build more robots for a resource than could be consumed
            if next_robot == Construction::OreRobot
                && state.ore_robots == blueprint.max_ore_use_per_turn
                || next_robot == Construction::ClayRobot
                    && state.clay_robots == blueprint.obsidian_robot_cost_clay
                || next_robot == Construction::ObsidianRobot
                    && state.obsidian_robots == blueprint.geode_robot_cost_obsidian
            {
                continue;
            }

            let mut search_state = state.clone();
            search_state.construction = next_robot;
            let num_geodes = find_most_geodes(blueprint, remaining_turns, search_state);
            max_geodes = max_geodes.max(num_geodes);
        }
    }
    return max_geodes;
}

fn main() {
    // Part1
    let input = fs::read_to_string("./inputs/day19.input").unwrap();
    let mut blueprints: Vec<Blueprint> = Vec::new();
    for line in input.lines() {
        let parts = line.split(' ').collect::<Vec<&str>>();
        let ore_robot_cost = parts[6].parse::<i32>().unwrap();
        let clay_robot_cost = parts[12].parse::<i32>().unwrap();
        let obsidian_robot_cost_ore = parts[18].parse::<i32>().unwrap();
        let obsidian_robot_cost_clay = parts[21].parse::<i32>().unwrap();
        let geode_robot_cost_ore = parts[27].parse::<i32>().unwrap();
        let geode_robot_cost_obsidian = parts[30].parse::<i32>().unwrap();

        let max_ore_use_per_turn = ore_robot_cost
            .max(clay_robot_cost.max(obsidian_robot_cost_ore.max(geode_robot_cost_ore)));
        blueprints.push(Blueprint {
            ore_robot_cost,
            clay_robot_cost,
            obsidian_robot_cost_ore,
            obsidian_robot_cost_clay,
            geode_robot_cost_ore,
            geode_robot_cost_obsidian,
            max_ore_use_per_turn,
        });
    }

    // Part 1
    let mut quality_level = 0;
    for (idx, blueprint) in blueprints.iter().enumerate() {
        // Start with enough ore to build an ore-robot ( the problem states you already have it but this way the search starts after the 'first' one is built)
        // (for the same reason we add an extra turn)
        let initial_state = State {
            ore: blueprint.ore_robot_cost,
            clay: 0,
            obsidian: 0,
            geodes: 0,
            ore_robots: 0,
            clay_robots: 0,
            obsidian_robots: 0,
            geode_robots: 0,
            construction: Construction::OreRobot,
        };
        let most_geods = find_most_geodes(&blueprint, 24 + 1, initial_state);
        quality_level += (idx as i32 + 1) * most_geods;
        println!("{}, {}, {}", idx + 1, quality_level, most_geods);
    }
    println!("Total quality level is {}", quality_level);

    // Part 2
    let mut result = 1;
    for (idx, blueprint) in blueprints[0..3].iter().enumerate() {
        // Start with enough ore to build an ore-robot ( the problem states you already have it but this way the search starts after the 'first' one is built)
        // (for the same reason we add an extra turn)
        let initial_state = State {
            ore: blueprint.ore_robot_cost,
            clay: 0,
            obsidian: 0,
            geodes: 0,
            ore_robots: 0,
            clay_robots: 0,
            obsidian_robots: 0,
            geode_robots: 0,
            construction: Construction::OreRobot,
        };
        let most_geods = find_most_geodes(&blueprint, 32 + 1, initial_state);
        result *= most_geods;
        println!("{}, {}, {}", idx + 1, result, most_geods);
    }
    println!("Part 2 result is {}", result);
}
