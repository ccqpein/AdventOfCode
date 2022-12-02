use tools::*;

fn day2(input: &Vec<String>) -> i64 {
    let (mut x, mut y) = ("", "");
    let mut total = 0;
    for line in input {
        let mut s = line.split(" ");
        x = s.next().unwrap();
        y = s.next().unwrap();

        if x == "A" {
            if y == "Y" {
                total += 2 + 6
            } else if y == "X" {
                total += 1 + 3
            } else {
                total += 3 + 0
            }
        }
        if x == "B" {
            if y == "Y" {
                total += 2 + 3
            } else if y == "X" {
                total += 1 + 0
            } else {
                total += 3 + 6
            }
        }
        if x == "C" {
            if y == "Y" {
                total += 2 + 0
            } else if y == "X" {
                total += 1 + 6
            } else {
                total += 3 + 3
            }
        }
    }
    total
}

fn day2_part2(input: &Vec<String>) -> i64 {
    let (mut x, mut y) = ("", "");
    let mut total = 0;
    for line in input {
        let mut s = line.split(" ");
        x = s.next().unwrap();
        y = s.next().unwrap();

        // rock 1
        // paper 2
        // sc 3

        if x == "A" {
            // rock
            if y == "Y" {
                total += 1 + 3
            } else if y == "X" {
                total += 3 + 0
            } else {
                total += 2 + 6
            }
        }
        if x == "B" {
            // paper
            if y == "Y" {
                total += 2 + 3
            } else if y == "X" {
                total += 1 + 0
            } else {
                total += 3 + 6
            }
        }
        if x == "C" {
            // scissors
            if y == "Y" {
                total += 3 + 3
            } else if y == "X" {
                total += 2 + 0
            } else {
                total += 1 + 6
            }
        }
    }
    total
}

fn main() {
    //let input = read_file_by_line("./inputs/day2_demo.input")
    let input = read_file_by_line("./inputs/day2.input");
    println!("{}", day2(&input));

    // let input = read_file_by_line("./inputs/day2.input");
    println!("{}", day2_part2(&input));
}
