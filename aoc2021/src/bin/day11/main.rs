use tools::*;

fn add1(map: &mut Vec<Vec<usize>>) {
    for i in 0..map.len() {
        for j in 0..map[0].len() {
            map[i][j] += 1
        }
    }
}

fn adjust_energy(map: &mut Vec<Vec<usize>>) {
    let mut flag = true;
    while flag {
        flag = false;
        for i in 0..map.len() {
            for j in 0..map[0].len() {
                if map[i][j] >= 10 {
                    flag = true;
                    map[i][j] = 0;
                    let (ii, jj) = (i as i32, j as i32);
                    for (x, y) in [
                        (ii - 1, jj - 1),
                        (ii - 1, jj),
                        (ii - 1, jj + 1),
                        (ii, jj - 1),
                        (ii, jj + 1),
                        (ii + 1, jj - 1),
                        (ii + 1, jj),
                        (ii + 1, jj + 1),
                    ] {
                        if x < 0 || y < 0 || x == map.len() as i32 || y == map[0].len() as i32 {
                            continue;
                        }

                        if map[x as usize][y as usize] != 0 {
                            map[x as usize][y as usize] += 1;
                        }
                    }
                }
            }
        }
    }
}

//fn adjust_energy(map:&mut )

fn print_input(input: &Vec<Vec<usize>>) {
    for l in input {
        println!("{:?}", l)
    }
}

fn get_all_flash(input: &Vec<Vec<usize>>) -> usize {
    input
        .iter()
        .map(|l| l.iter().filter(|d| **d == 0).count())
        .sum()
}

fn part1(input: &Vec<String>, step: usize) -> usize {
    let mut input: Vec<Vec<usize>> = input
        .iter()
        .map(|l| {
            l.chars()
                .map(|c| c.to_string().parse::<usize>().unwrap())
                .collect()
        })
        .collect();

    let how_many = input.len() * input[0].len();
    let mut result = 0;
    for s in 0..step {
        add1(&mut input);
        adjust_energy(&mut input);
        let all = get_all_flash(&input);
        if all == 100 {
            println!("at step: {}", s + 1);
            break;
        }
        result += all
    }

    result
}

fn adjust_energy_v2(m: &mut Map<usize>) {
    let mut flag = true;
    while flag {
        flag = false;
        let mut this_round_cache = vec![];
        for ((i, j), v) in m.iter_mut() {
            if *v >= 10 {
                flag = true;
                *v = 0;
                this_round_cache.push((i, j));
            }
        }
        for center in this_round_cache {
            for ((r, c), v) in m.get_around(center).collect::<Vec<_>>() {
                if v != 0 {
                    *m.get_mut(r, c).unwrap() += 1;
                }
            }
        }
    }
}

fn part1_v2(input: &Vec<String>, step: usize) -> usize {
    let mut input: Vec<Vec<usize>> = input
        .iter()
        .map(|l| {
            l.chars()
                .map(|c| c.to_string().parse::<usize>().unwrap())
                .collect()
        })
        .collect();

    let mut map = Map::from(input);
    let mut result = 0;
    for s in 0..step {
        map.iter_mut().for_each(|(_, e)| *e += 1);
        adjust_energy_v2(&mut map);
        let all = map.iter().filter(|(_, v)| **v == 0).count();
        if all == 100 {
            println!("at step: {}", s + 1);
            break;
        }
        result += all
    }

    result
}

fn main() {
    let input = read_file_by_line("./src/bin/day11/day11.input");
    //let input = read_file_by_line("./src/bin/day11/day11_demo.input");
    println!("{}", part1(&input, 100));
    println!("{}", part1(&input, usize::MAX - 1));
    println!("{}", part1_v2(&input, 100));
    println!("{}", part1_v2(&input, usize::MAX - 1));
    //println!("{}", part2(&input));
}
