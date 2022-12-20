use tools::*;

struct Num {
    n: i64,
    moved: bool,
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
        println!("new ind: {}, {}", new_i, list[i].n);
        let mut to_move = list.remove(i);
        to_move.moved = true;
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
    //let input = read_file_by_line("./inputs/day20_demo.input")
    let input = read_file_by_line("./inputs/day20.input");
    println!("{}", day20(&input));
    //println!("{}", day20(&input, 2));
}
