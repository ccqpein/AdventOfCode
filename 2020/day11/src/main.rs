use tools::read_file_by_line;

fn parser(l: &Vec<String>) -> Vec<Vec<char>> {
    l.iter()
        .map(|ll| ll.chars().collect::<Vec<char>>())
        .collect()
}

fn all_around(this: &Vec<Vec<char>>, r: i32, c: i32, rlen: i32, clen: i32) -> Vec<char> {
    [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ]
    .iter()
    .map(|(ra, ca)| ((r + ra), (c + ca)))
    .filter(|(r, c)| *r >= 0 && *r < rlen && *c >= 0 && *c < clen)
    .map(|(ra, ca)| this[ra as usize][ca as usize])
    .collect()
}

fn handle(this: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let row_len = this.len();
    let col_len = this[0].len();

    let mut cache = vec![];
    let mut result = vec![];
    for r in 0..row_len {
        cache.clear();
        for c in 0..col_len {
            let around = all_around(&this, r as i32, c as i32, row_len as i32, col_len as i32);
            if around.iter().all(|&s| s == 'L' || s == '.') && this[r][c] == 'L' {
                cache.push('#')
            } else if this[r][c] == '#' && around.iter().filter(|&s| *s == '#').count() >= 4 {
                cache.push('L')
            } else {
                cache.push(this[r][c])
            }
        }
        result.push(cache.clone());
    }
    result
}

fn all_around2(this: &Vec<Vec<char>>, r: i32, c: i32, rlen: i32, clen: i32) -> Vec<char> {
    let mut result = vec![];
    for (rad, cad) in [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ]
    .iter()
    {
        let mut rr = r + rad;
        let mut cc = c + cad;
        loop {
            if rr >= 0 && rr < rlen && cc >= 0 && cc < clen {
                if this[rr as usize][cc as usize] != '.' {
                    result.push(this[rr as usize][cc as usize]);
                    break;
                }
                rr += rad;
                cc += cad
            } else {
                result.push('.');
                break;
            }
        }
    }
    result
}

fn handle2(this: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let row_len = this.len();
    let col_len = this[0].len();

    let mut cache = vec![];
    let mut result = vec![];
    for r in 0..row_len {
        cache.clear();
        for c in 0..col_len {
            let around = all_around2(&this, r as i32, c as i32, row_len as i32, col_len as i32);
            if around.iter().all(|&s| s == 'L' || s == '.') && this[r][c] == 'L' {
                cache.push('#')
            } else if this[r][c] == '#' && around.iter().filter(|&s| *s == '#').count() >= 5 {
                cache.push('L')
            } else {
                cache.push(this[r][c])
            }
        }
        result.push(cache.clone());
    }
    result
}

fn part1(input: &Vec<String>) -> usize {
    let mut this = parser(input);
    let mut last = this.clone();

    loop {
        this = handle(&this);
        // this.iter().for_each(|l| {
        //     l.iter().for_each(|c| print!("{}", c));
        //     println!("");
        // });
        // println!("");
        if this == last {
            break;
        } else {
            last = this.clone();
        }
    }

    this.iter()
        .map(|l| l.iter().filter(|c| **c == '#').count())
        .sum()
}

fn part2(input: &Vec<String>) -> usize {
    let mut this = parser(input);
    let mut last = this.clone();

    loop {
        this = handle2(&this);
        if this == last {
            break;
        } else {
            last = this.clone();
        }
    }

    this.iter()
        .map(|l| l.iter().filter(|c| **c == '#').count())
        .sum()
}

fn main() {
    let input = read_file_by_line(String::from("./src/day11.input"));
    println!("part1: {}", part1(&input));
    println!("part2: {}", part2(&input));
}
