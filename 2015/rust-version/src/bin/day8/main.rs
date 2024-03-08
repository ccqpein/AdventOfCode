use std::{io::Cursor, str::Chars};

use tools::*;

fn escape(s: &str) -> usize {
    let mut c = s.chars();
    c.next(); // first "
    let mut result = 0;
    loop {
        if let Some(cc) = c.next() {
            match cc {
                '\\' => {
                    if c.next().unwrap() == 'x' {
                        result += 1;
                        c.next();
                        c.next();
                    } else {
                        result += 1;
                        //c.next();
                    }
                }
                _ => result += 1,
            }
        } else {
            break;
        }
    }
    //dbg!(result - 1);
    result - 1
}

fn day8(input: &[String]) -> usize {
    input.iter().map(|l| l.len() - escape(l)).sum()
}

fn encoding(s: &str) -> usize {
    let mut c = s.chars();
    let mut result = 1;
    loop {
        if let Some(cc) = c.next() {
            match cc {
                '\"' | '\\' => {
                    result += 2;
                }

                _ => result += 1,
            }
        } else {
            break;
        }
    }
    dbg!(result + 1);
    result + 1
}

fn day8_2(input: &[String]) -> usize {
    input.iter().map(|l| encoding(l) - l.len()).sum()
}

fn main() {
    let input = read_file_by_line("../inputs/day8.input");
    //let input = read_file_by_line("../inputs/day8_demo.input");
    // for b in input[2].chars() {
    //     println!("{}", b);
    // }
    // println!("{}", input[2].len());

    println!("{:?}", day8(&input));
    println!("{:?}", day8_2(&input));
}

// macro_rules! ss {
//     ($l:expr) => {
//         format!("{}", $l)
//     };
// }

#[cfg(test)]
mod tests {
    use tools::read_file_by_line;

    #[test]
    fn test_a() {
        assert_eq!(6, br#""\x27""#.len());
        assert_eq!(1, "\x27".len());

        assert_eq!(2, br#""""#.len());
        assert_eq!(0, "".len());

        assert_eq!(10, r#""aaa\"aaa""#.len());
        assert_eq!(7, "aaa\"aaa".len());
        //assert_eq!(7, format!("{}", r#""aaa\"aaa""#).len());

        assert_eq!(43, r#""byc\x9dyxuafof\\\xa6uf\\axfozomj\\olh\x6a""#.len())
    }

    #[test]
    fn test_m() {
        let input = read_file_by_line("../inputs/day8_demo.input");
        dbg!(&input[2]);
        dbg!(String::from(input[2].clone()).len());
    }
}
