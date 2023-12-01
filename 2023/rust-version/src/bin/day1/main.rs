use tools::*;

fn day1(lines: &[String]) -> i32 {
    let mut result = vec![];
    for line in lines {
        let mut first = None;
        let mut last = String::new();
        for c in line.as_bytes() {
            // how to parse to int
            match c {
                b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' | b'0' => {
                    if first.is_none() {
                        //dbg!((c - 48).to_string());
                        first = Some((c - 48).to_string());
                    }

                    last = (c - 48).to_string();
                }
                _ => {}
            }
        }

        let a = format!("{}{}", first.unwrap(), last);
        result.push(a.parse::<i32>().unwrap())
    }

    //dbg!(&result);
    result.iter().sum()
}

fn day1_2(lines: &[String]) -> i32 {
    let mut result = vec![];
    for line in lines {
        let mut inside_line = line.to_string();
        let mut first = None;
        let mut last = String::new();
        while !inside_line.is_empty() {
            // how to parse to int
            match inside_line.as_bytes()[0] {
                b'1' => {
                    if first.is_none() {
                        first = Some("one".to_string());
                    }
                    last = "one".to_string();
                }
                b'2' => {
                    if first.is_none() {
                        first = Some("two".to_string());
                    }
                    last = "two".to_string();
                }
                b'3' => {
                    if first.is_none() {
                        first = Some("three".to_string());
                    }
                    last = "three".to_string();
                }
                b'4' => {
                    if first.is_none() {
                        first = Some("four".to_string());
                    }
                    last = "four".to_string();
                }
                b'5' => {
                    if first.is_none() {
                        first = Some("five".to_string());
                    }
                    last = "five".to_string();
                }
                b'6' => {
                    if first.is_none() {
                        first = Some("six".to_string());
                    }
                    last = "six".to_string();
                }
                b'7' => {
                    if first.is_none() {
                        first = Some("seven".to_string());
                    }
                    last = "seven".to_string();
                }
                b'8' => {
                    if first.is_none() {
                        first = Some("eight".to_string());
                    }
                    last = "eight".to_string();
                }
                b'9' => {
                    if first.is_none() {
                        first = Some("nine".to_string());
                    }
                    last = "nine".to_string();
                }
                b'0' => {
                    if first.is_none() {
                        first = Some("zero".to_string());
                    }
                    last = "zero".to_string();
                }
                _ => {
                    for a in vec![
                        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                        "zero",
                    ] {
                        if inside_line.starts_with(a) {
                            if first.is_none() {
                                first = Some(a.to_string())
                            }
                            last = a.to_string();
                            break;
                        }
                    }
                }
            }

            //dbg!(&inside_line);
            inside_line = if let Some(ss) = inside_line.get(1..) {
                ss.to_string()
            } else {
                break;
            };
        }

        let mut cache = vec![];
        for i in [first.unwrap(), last] {
            match i.as_str() {
                "one" => cache.push(1),
                "two" => cache.push(2),
                "three" => cache.push(3),
                "four" => cache.push(4),
                "five" => cache.push(5),
                "six" => cache.push(6),
                "seven" => cache.push(7),
                "eight" => cache.push(8),
                "nine" => cache.push(9),
                "zero" => cache.push(0),
                _ => unreachable!(),
            }
        }
        result.push(cache[0] * 10 + cache[1])
    }

    //dbg!(&result);
    result.iter().sum()
}

fn main() {
    //let input = read_file_by_line("../inputs/day1_demo.input");
    let input = read_file_by_line("../inputs/day1.input");
    //println!("{:?}", input);
    println!("{}", day1(&input));
    println!("{}", day1_2(&input));
}
