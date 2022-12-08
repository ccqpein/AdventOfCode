use std::collections::HashSet;

use tools::*;

fn day8(input: &Vec<String>) -> usize {
    let tree_map = Map::from(
        input
            .iter()
            .map(|l| {
                l.chars()
                    .map(|c| c.to_string().parse::<i32>().unwrap())
                    .collect()
            })
            .collect::<Vec<Vec<i32>>>(),
    );

    let mut count_set = HashSet::new();

    let left_edge = (0..tree_map.row_len()).into_iter().map(|r| (r, 0));
    let right_edge = (0..tree_map.row_len())
        .into_iter()
        .map(|r| (r, tree_map.col_len() - 1));

    let upper_edge = (0..tree_map.col_len()).into_iter().map(|c| (0, c));
    let bottom_edge = (0..tree_map.col_len())
        .into_iter()
        .map(|c| (tree_map.row_len() - 1, c));

    left_edge.for_each(|coop| part1_helper(tree_map.go_through_right(coop), -1, &mut count_set));
    right_edge.for_each(|coop| part1_helper(tree_map.go_through_left(coop), -1, &mut count_set));
    upper_edge.for_each(|coop| part1_helper(tree_map.go_through_down(coop), -1, &mut count_set));
    bottom_edge.for_each(|coop| part1_helper(tree_map.go_through_up(coop), -1, &mut count_set));

    count_set.len()
}

fn part1_helper(
    trees: impl Iterator<Item = ((usize, usize), i32)>,
    mut largest: i32,
    record: &mut HashSet<(usize, usize)>,
) {
    for (coop, t) in trees {
        if t > largest {
            largest = t;
            record.insert(coop);
        }
    }
}

fn day8_part2(input: &Vec<String>) -> usize {
    let tree_map = Map::from(
        input
            .iter()
            .map(|l| {
                l.chars()
                    .map(|c| c.to_string().parse::<i32>().unwrap())
                    .collect()
            })
            .collect::<Vec<Vec<i32>>>(),
    );

    tree_map
        .iter()
        .map(|(coop, t)| {
            let up = tree_map.go_through_up(coop);
            let down = tree_map.go_through_down(coop);
            let left = tree_map.go_through_left(coop);
            let right = tree_map.go_through_right(coop);

            part2_helper(up, *t)
                + part2_helper(down, *t)
                + part2_helper(left, *t)
                + part2_helper(right, *t)
        })
        .sum()
}

fn part2_helper(trees: impl Iterator<Item = ((usize, usize), i32)>, this_tree: i32) -> usize {
    let mut count = 0;
    for (_, t) in trees {
        if t >= this_tree {
            return count + 1;
        } else {
            count += 1
        }
    }
    count
}

fn main() {
    //let input = read_file_by_line("./inputs/day8_demo.input")
    let input = read_file_by_line("./inputs/day8.input");
    println!("{}", day8(&input));
    println!("{}", day8_part2(&input));
}
