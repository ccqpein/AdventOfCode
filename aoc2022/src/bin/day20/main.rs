use std::{cell::RefCell, rc::Rc};

use tools::*;
const key: i64 = 811589153;

struct Num {
    n: i64,
    moved: bool,
}

struct Entry {
    n: i64,
    idx: RefCell<usize>,
}

fn day20(input: &Vec<String>) -> i64 {
    let mut input = input
        .iter()
        .map(|v| Num {
            n: v.parse::<i64>().unwrap(),
            moved: false,
        })
        .collect();

    move_entries(&mut input);
    get_score(&input)
}

fn day20_part2(input: &Vec<String>) -> i64 {
    let mut order = Vec::new();
    let mut array = Vec::new();
    for (ind, v) in input.iter().enumerate() {
        let n: i64 = v.parse::<i64>().unwrap();
        // Both lists have a reference counter to the same struct
        let entry = Rc::new(Entry {
            n: n * key,
            idx: RefCell::new(ind),
        });
        order.push(entry.clone());
        array.push(entry);
    }

    (0..10).for_each(|_| move_entries_p2(&order, &mut array));

    get_score(
        &array
            .iter()
            .map(|e| Num {
                n: e.n,
                moved: false,
            })
            .collect(),
    )
}

fn move_entries(list: &mut Vec<Num>) {
    let mut i = 0;
    let n = list.len();
    while i < n {
        if list[i].moved {
            i += 1;
            continue;
        }
        // Wrap at n - 1 because magic
        let wrap = n as i64 - 1;
        let mut new_i = (i as i64 + list[i].n) % wrap;
        if new_i < 0 {
            new_i = wrap + new_i
        }
        //println!("new ind: {}, {}", new_i, list[i].n);
        let mut to_move = list.remove(i);
        to_move.moved = true;
        list.insert(new_i as usize, to_move);
    }
}

fn move_entries_p2(order: &Vec<Rc<Entry>>, list: &mut Vec<Rc<Entry>>) {
    let wrap = order.len() as i64 - 1;
    // Iterate in the original order
    for e in order {
        let mut old_i = e.idx.borrow_mut();
        // Calculate new index
        let mut new_i = (*old_i as i64 + e.n) % wrap;
        if new_i < 0 {
            new_i = wrap + new_i
        }
        let new_i = new_i as usize;

        // Change indices of all moved entries
        if new_i < *old_i {
            for i in new_i..*old_i {
                list[i].idx.replace_with(|&mut old| old + 1);
            }
        } else if *old_i < new_i {
            for i in (*old_i + 1)..=new_i {
                list[i].idx.replace_with(|&mut old| old - 1);
            }
        }

        // Move the current entry in the list
        let to_move = list.remove(*old_i);
        *old_i = new_i;
        list.insert(new_i as usize, to_move);
    }
}

fn get_score(list: &Vec<Num>) -> i64 {
    let pos0 = list
        .iter()
        .position(|e| e.n == 0)
        .expect("List should contain a 0");
    let n = list.len();

    list[(pos0 + 1000) % n].n + list[(pos0 + 2000) % n].n + list[(pos0 + 3000) % n].n
}

fn main() {
    //let input = read_file_by_line("./inputs/day20_demo.input");
    let input = read_file_by_line("./inputs/day20.input");
    println!("{}", day20(&input));
    println!("{}", day20_part2(&input));
}
