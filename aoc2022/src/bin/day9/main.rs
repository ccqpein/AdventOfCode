use std::collections::HashSet;

use tools::*;

fn day9(input: &Vec<String>) -> usize {
    let commands = input
        .iter()
        .map(|line| {
            let mut a = line.split(' ');
            vec![a.next().unwrap(); a.next().unwrap().parse::<usize>().unwrap()]
        })
        .flatten();

    let mut head = (0_i32, 0_i32);
    let mut tail = (0_i32, 0_i32);

    let mut set = HashSet::new();
    for comm in commands {
        one_move(&mut head, comm);
        tail = move_close(&head, &tail);
        set.insert(tail);
    }

    set.len()
}

fn day9_part2(input: &Vec<String>) -> usize {
    let commands = input
        .iter()
        .map(|line| {
            let mut a = line.split(' ');
            vec![a.next().unwrap(); a.next().unwrap().parse::<usize>().unwrap()]
        })
        .flatten();

    let mut snake = vec![(0, 0); 10];
    let mut set = HashSet::new();

    for comm in commands {
        one_move(snake.get_mut(0).unwrap(), comm);
        for i in 1..=9 {
            snake[i] = move_close(&snake[i - 1], &snake[i]);
        }
        set.insert(snake[9]);
    }
    set.len()
}

fn move_close(head: &(i32, i32), tail: &(i32, i32)) -> (i32, i32) {
    let mut row_offset = 0;
    let mut col_offset = 0;

    if (tail.0 - head.0).abs() <= 1 && (tail.1 - head.1).abs() <= 1 {
        return *tail;
    }

    if (head.0 - tail.0) < 0 {
        row_offset = -1;
    } else if (head.0 - tail.0) > 0 {
        row_offset = 1;
    }

    if (head.1 - tail.1) < 0 {
        col_offset = -1;
    } else if (head.1 - tail.1) > 0 {
        col_offset = 1;
    }

    // new tail
    (tail.0 + row_offset, tail.1 + col_offset)
}

fn one_move(coop: &mut (i32, i32), comm: &str) {
    match comm {
        "R" => {
            coop.1 += 1;
        }
        "L" => {
            coop.1 -= 1;
        }
        "U" => {
            coop.0 += 1;
        }
        "D" => {
            coop.0 -= 1;
        }
        _ => unreachable!(),
    }
}

fn main() {
    //let input = read_file_by_line("./inputs/day9_demo.input");
    let input = read_file_by_line("./inputs/day9.input");
    println!("{}", day9(&input));
    println!("{}", day9_part2(&input));
}
