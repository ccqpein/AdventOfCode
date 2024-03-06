use regex::Regex;
use tools::*;

#[derive(Debug, PartialEq, Eq)]
pub struct Square {
    on: bool,
    // left upper corner
    lu: (i64, i64),
    // right down corner
    rd: (i64, i64),
}

pub fn joint_square(a: &Square, b: &Square) -> Option<Square> {
    let x1 = a.lu.0.max(b.lu.0);
    let y1 = a.lu.1.max(b.lu.1);

    if (x1 >= a.rd.0 && y1 >= a.rd.1) || (x1 >= b.rd.0 && y1 >= b.rd.1) {
        return None;
    }

    let x2 = a.rd.0.min(b.rd.0);
    let y2 = a.rd.1.min(b.rd.1);

    Some(Square {
        on: false,
        lu: (x1, y1),
        rd: (x2, y2),
    })
}

fn parse(s: &str) -> (&str, Square) {
    let re = Regex::new(r"^(.+) (\d+),(\d+) through (\d+),(\d+)$").unwrap();
    let (_, [comm, x1, y1, x2, y2]) = re.captures(s).unwrap().extract();
    (
        comm,
        Square {
            on: false,
            lu: (x1.parse().unwrap(), y1.parse().unwrap()),
            rd: (x2.parse().unwrap(), y2.parse().unwrap()),
        },
    )
}

fn day6(input: &[String]) -> usize {
    let re = Regex::new(r"^(.+) (\d+),(\d+) through (\d+),(\d+)$").unwrap();
    let mut whole_map = [[0; 1000]; 1000];
    for line in input {
        let (_, [comm, x1, y1, x2, y2]) = re.captures(line).unwrap().extract();
        let (x1, y1, x2, y2) = (
            x1.parse::<usize>().unwrap(),
            y1.parse::<usize>().unwrap(),
            x2.parse::<usize>().unwrap(),
            y2.parse::<usize>().unwrap(),
        );
        match comm {
            "turn on" => {
                for y in y1.min(y2)..=y1.max(y2) {
                    for x in x1.min(x2)..=x1.max(x2) {
                        whole_map[y][x] = 1;
                    }
                }
            }
            "turn off" => {
                for y in y1.min(y2)..=y1.max(y2) {
                    for x in x1.min(x2)..=x1.max(x2) {
                        whole_map[y][x] = 0;
                    }
                }
            }
            "toggle" => {
                for y in y1.min(y2)..=y1.max(y2) {
                    for x in x1.min(x2)..=x1.max(x2) {
                        whole_map[y][x] ^= 1;
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    whole_map
        .iter()
        .map(|l| l.iter().sum::<usize>())
        .sum::<usize>()
}

fn day6_2(input: &[String]) -> usize {
    let re = Regex::new(r"^(.+) (\d+),(\d+) through (\d+),(\d+)$").unwrap();
    let mut whole_map = [[0; 1000]; 1000];
    for line in input {
        let (_, [comm, x1, y1, x2, y2]) = re.captures(line).unwrap().extract();
        let (x1, y1, x2, y2) = (
            x1.parse::<usize>().unwrap(),
            y1.parse::<usize>().unwrap(),
            x2.parse::<usize>().unwrap(),
            y2.parse::<usize>().unwrap(),
        );
        match comm {
            "turn on" => {
                for y in y1.min(y2)..=y1.max(y2) {
                    for x in x1.min(x2)..=x1.max(x2) {
                        whole_map[y][x] += 1;
                    }
                }
            }
            "turn off" => {
                for y in y1.min(y2)..=y1.max(y2) {
                    for x in x1.min(x2)..=x1.max(x2) {
                        if whole_map[y][x] != 0 {
                            whole_map[y][x] -= 1;
                        }
                    }
                }
            }
            "toggle" => {
                for y in y1.min(y2)..=y1.max(y2) {
                    for x in x1.min(x2)..=x1.max(x2) {
                        whole_map[y][x] += 2;
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    whole_map
        .iter()
        .map(|l| l.iter().sum::<usize>())
        .sum::<usize>()
}

fn main() {
    let input = read_file_by_line("../inputs/day6.input");
    //let input = vec!["turn on 0,0 through 999,999".to_string()];
    println!("{:?}", day6(&input));

    //let input = vec!["toggle 0,0 through 999,999".to_string()];
    //let input = vec!["turn on 0,0 through 0,0".to_string()];

    println!("{:?}", day6_2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_joint_square() {
        let a = joint_square(
            &Square {
                on: false,
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                on: false,
                lu: (0, 0),
                rd: (10, 10),
            },
        );

        assert_eq!(
            a.unwrap(),
            Square {
                on: false,
                lu: (0, 0),
                rd: (10, 10),
            },
        );

        let a = joint_square(
            &Square {
                on: false,
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                on: false,
                lu: (5, 5),
                rd: (11, 11),
            },
        );

        assert_eq!(
            a.unwrap(),
            Square {
                on: false,
                lu: (5, 5),
                rd: (10, 10),
            },
        );

        let a = joint_square(
            &Square {
                on: false,
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                on: false,
                lu: (5, 5),
                rd: (9, 9),
            },
        );

        assert_eq!(
            a.unwrap(),
            Square {
                on: false,
                lu: (5, 5),
                rd: (9, 9),
            },
        );

        let a = joint_square(
            &Square {
                on: false,
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                on: false,
                lu: (5, 5),
                rd: (9, 11),
            },
        );

        assert_eq!(
            a.unwrap(),
            Square {
                on: false,
                lu: (5, 5),
                rd: (9, 10),
            },
        );

        let a = joint_square(
            &Square {
                on: false,
                lu: (0, 0),
                rd: (5, 5),
            },
            &Square {
                on: false,
                lu: (5, 5),
                rd: (9, 11),
            },
        );

        assert_eq!(a, None);
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("turn off 278,258 through 367,386"),
            (
                "turn off",
                Square {
                    on: false,
                    lu: (278, 258),
                    rd: (367, 386)
                }
            )
        )
    }
}
