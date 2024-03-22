use rayon::prelude::*;
use tools::*;

fn day18(input: &[String], step: usize, p2: bool) -> usize {
    let mut m: Map<_> = input
        .into_iter()
        .map(|l| l.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>()
        .into();

    if p2 {
        for c in m.four_corners() {
            *m.get_mut(c.0, c.1).unwrap() = '#';
        }
    }

    let mut bucket = vec![];
    let fc = m.four_corners();

    for _ in 0..step {
        for (coop, c) in m.iter() {
            if p2 && fc.contains(&coop) {
                continue;
            }

            match c {
                '#' => {
                    let n = m.get_around(coop).filter(|(_, a)| **a == '#').count();
                    if n != 2 && n != 3 {
                        //*m.get_mut(coop.0, coop.1).unwrap() = '.';
                        //*c = '.';
                        bucket.push((coop, '.'))
                    }
                }
                '.' => {
                    let n = m.get_around(coop).filter(|(_, a)| **a == '#').count();
                    if n == 3 {
                        bucket.push((coop, '#'))
                    }
                }
                _ => {}
            }
        }

        for ((r, c), x) in bucket.iter() {
            *m.get_mut(*r, *c).unwrap() = *x
        }
        //println!("{}", m);
        bucket.clear()
    }

    m.iter().filter(|(_, c)| **c == '#').count()
}

fn main() {
    let input = read_file_by_line("../inputs/day18.input");
    /*
        let input = r#".#.#.#
    ...##.
    #....#
    ..#...
    #.#..#
    ####.."#
            .lines()
            .map(|s| s.to_string())
        .collect::<Vec<_>>();
        */

    println!("1: {:?}", day18(&input, 100, false));
    println!("2: {:?}", day18(&input, 100, true));
}
