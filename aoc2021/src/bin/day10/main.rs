use tools::*;

fn shouldbe(c: char) -> char {
    match c {
        ']' => '[',
        ')' => '(',
        '}' => '{',
        '>' => '<',
        _ => {
            panic!()
        }
    }
}

fn point(c: char) -> i32 {
    match c {
        ']' => 57,
        ')' => 3,
        '}' => 1197,
        '>' => 25137,
        _ => {
            panic!()
        }
    }
}

fn part1(input: &Vec<String>) -> i32 {
    let mut result = 0;
    for l in input {
        let mut stack = vec![];
        let mut lc = l.chars();
        for c in lc {
            match c {
                a @ '[' | a @ '(' | a @ '{' | a @ '<' => {
                    stack.push(a);
                }
                a @ ']' | a @ ')' | a @ '}' | a @ '>' => {
                    if let Some(b) = stack.last() {
                        if *b == shouldbe(a) {
                            stack.pop();
                        } else {
                            result += point(a);
                            break;
                        }
                    } else {
                        result += point(a);
                        break;
                    }
                }
                _ => {
                    panic!()
                }
            }
        }
    }
    result
}

fn point2(stack: Vec<char>) -> usize {
    let mut result = 0;
    for c in stack.into_iter().rev() {
        result *= 5;
        result += match c {
            '[' => 2,
            '(' => 1,
            '{' => 3,
            '<' => 4,
            _ => {
                panic!()
            }
        }
    }
    result
}

fn part2(input: &Vec<String>) -> usize {
    let mut result = vec![];
    'line: for l in input {
        let mut stack = vec![];
        let mut lc = l.chars();
        for c in lc {
            match c {
                a @ '[' | a @ '(' | a @ '{' | a @ '<' => {
                    stack.push(a);
                }
                a @ ']' | a @ ')' | a @ '}' | a @ '>' => {
                    if let Some(b) = stack.last() {
                        if *b == shouldbe(a) {
                            stack.pop();
                        } else {
                            continue 'line;
                        }
                    } else {
                        continue 'line;
                    }
                }
                _ => {
                    panic!()
                }
            }
        }
        if stack.len() != 0 {
            result.push(point2(stack))
        }
    }
    result.sort();
    result[result.len() / 2]
}

fn main() {
    let input = read_file_by_line("./src/bin/day10/day10.input");
    //let input = read_file_by_line("./src/bin/day10/day10_demo.input");
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
