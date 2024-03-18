use regex::*;
use tools::*;

fn day14(input: &[String], s: usize) -> usize {
    let re = Regex::new(
        r#"^(.+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$"#,
    )
    .unwrap();

    let mut result = vec![];
    for line in input {
        let (_, [name, sp, se, rs]) = re.captures(line).unwrap().extract();
        let sp = sp.parse::<usize>().unwrap();
        let se = se.parse::<usize>().unwrap();
        let rs = rs.parse::<usize>().unwrap();

        let dis = (s / (se + rs)) * (sp * se) + {
            if (s % (se + rs)) <= se {
                (s % (se + rs)) * sp
            } else {
                sp * se
            }
        };
        result.push((name, dis))
    }
    dbg!(&result);
    result.iter().max_by(|x, y| x.1.cmp(&y.1)).unwrap().1
}

fn day14_2(input: &[String], s: usize) -> usize {
    let re = Regex::new(
        r#"^(.+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$"#,
    )
    .unwrap();

    let mut all_stars = vec![];

    for line in input {
        let (_, [_, sp, se, rs]) = re.captures(line).unwrap().extract();
        let sp = sp.parse::<usize>().unwrap();
        let se = se.parse::<usize>().unwrap();
        let rs = rs.parse::<usize>().unwrap();
        all_stars.push((sp, se, rs));
    }

    let mut result: Vec<(usize, usize)> = vec![(0, 0); all_stars.len()];

    for i in 1..=s {
        for (ind, (sp, se, rs)) in all_stars.iter().enumerate() {
            if i % (se + rs) <= *se && i % (se + rs) > 0 {
                result[ind].0 += *sp
            }
        }

        let max_dis = result.iter().max_by(|a, b| a.0.cmp(&b.0)).unwrap().0;
        //dbg!(&max_dis);
        for (dis, point) in &mut result {
            if *dis == max_dis {
                *point += 1;
            }
        }

        //dbg!(&result);
    }

    result.iter().max_by(|x, y| x.1.cmp(&y.1)).unwrap().1
}

fn main() {
    let input = read_file_by_line("../inputs/day14.input");
    //let input = read_file_by_line("../inputs/day14_demo.input");
    println!("1: {:?}", day14(&input, 2503));
    println!("1: {:?}", day14_2(&input, 2503));
}
