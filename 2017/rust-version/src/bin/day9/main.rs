use std::str::Chars;

use tools::*;

fn garbage(chars: &mut Chars) -> usize {
    let mut count = 0;
    loop {
        match chars.next() {
            Some(c) if c == '>' => break,
            Some(c) if c == '!' => {
                chars.next();
            }
            None => break,
            _ => count += 1,
        }
    }
    count
}

fn groups(level: usize, chars: &mut Chars) -> (usize, usize) {
    let mut deeper_level = level;
    let mut g_count = 0;

    loop {
        match chars.next() {
            Some(c) if c == '}' => break,
            Some(c) if c == '{' => {
                let (l, g) = groups(level + 1, chars);
                deeper_level += l;
                g_count += g;
            }
            Some(c) if c == '<' => g_count += garbage(chars),
            None => break,
            _ => continue,
        }
    }
    (deeper_level, g_count)
}

fn main() {
    assert_eq!(groups(0, &mut "{}".chars()).0, 1);
    assert_eq!(groups(0, &mut "{{{}}}".chars()).0, 6);
    assert_eq!(groups(0, &mut "{{},{}}".chars()).0, 5);
    assert_eq!(groups(0, &mut "{{{},{},{{}}}}".chars()).0, 16);

    assert_eq!(groups(0, &mut "{{<a!>},{<a!>},{<a!>},{<ab>}}".chars()).0, 3);
    assert_eq!(groups(0, &mut "{{<!!>},{<!!>},{<!!>},{<!!>}}".chars()).0, 9);
    assert_eq!(groups(0, &mut "{{<ab>},{<ab>},{<ab>},{<ab>}}".chars()).0, 9);
    assert_eq!(groups(0, &mut "{<a>,<a>,<a>,<a>}".chars()).0, 1);

    let input = read_file_by_line("../inputs/day9.input");
    dbg!(groups(0, &mut input[0].chars()));
}
