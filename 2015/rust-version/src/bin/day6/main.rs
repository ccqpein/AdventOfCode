use regex::Regex;
use tools::*;

#[derive(Debug, PartialEq, Eq)]
pub struct Square {
    // left upper corner
    lu: (i64, i64),
    // right down corner
    rd: (i64, i64),
}

pub fn joint_square(a: &Square, b: &Square) -> Square {
    let x1 = a.lu.0.max(b.lu.0);
    let y1 = a.lu.1.max(b.lu.1);

    let x2 = a.rd.0.min(b.rd.0);
    let y2 = a.rd.1.min(b.rd.1);

    Square {
        lu: (x1, y1),
        rd: (x2, y2),
    }
}

fn parse(s: &str) -> (&str, Square) {
    let re = Regex::new(r"^(.+) (\d+),(\d+) through (\d+),(\d+)$").unwrap();
    let (_, [comm, x1, y1, x2, y2]) = re.captures(s).unwrap().extract();
    (
        comm,
        Square {
            lu: (x1.parse().unwrap(), y1.parse().unwrap()),
            rd: (x2.parse().unwrap(), y2.parse().unwrap()),
        },
    )
}

fn day6(input: &[String]) -> usize {
    input.iter().map(|line| parse(line));
    0
}

fn main() {
    let input = read_file_by_line("../inputs/day6.input");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_joint_square() {
        let a = joint_square(
            &Square {
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                lu: (0, 0),
                rd: (10, 10),
            },
        );

        assert_eq!(
            a,
            Square {
                lu: (0, 0),
                rd: (10, 10),
            },
        );

        let a = joint_square(
            &Square {
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                lu: (5, 5),
                rd: (11, 11),
            },
        );

        assert_eq!(
            a,
            Square {
                lu: (5, 5),
                rd: (10, 10),
            },
        );

        let a = joint_square(
            &Square {
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                lu: (5, 5),
                rd: (9, 9),
            },
        );

        assert_eq!(
            a,
            Square {
                lu: (5, 5),
                rd: (9, 9),
            },
        );

        let a = joint_square(
            &Square {
                lu: (0, 0),
                rd: (10, 10),
            },
            &Square {
                lu: (5, 5),
                rd: (9, 11),
            },
        );

        assert_eq!(
            a,
            Square {
                lu: (5, 5),
                rd: (9, 10),
            },
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("turn off 278,258 through 367,386"),
            (
                "turn off",
                Square {
                    lu: (278, 258),
                    rd: (367, 386)
                }
            )
        )
    }
}
