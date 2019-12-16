fn parse_step(s: &str) -> (String, i32) {
    let (a, b) = s.split_at(1);
    (a.to_string(), b.parse().unwrap())
}

#[derive(Clone, Copy)]
struct Line {
    point1: (i32, i32),
    point2: (i32, i32),
}

impl Line {
    fn new(point1: (i32, i32), point2: (i32, i32)) -> Self {
        Line {
            point1: point1,
            point2: point2,
        }
    }

    fn intersection_point(&self, other: &Self) -> Option<(i32, i32)> {
        if vec![self.point1.0, self.point2.0].iter().max()
            < vec![other.point1.0, other.point2.0].iter().min()
        {
            return None;
        }

        if vec![self.point1.0, self.point2.0].iter().min()
            > vec![other.point1.0, other.point2.0].iter().max()
        {
            return None;
        }

        if vec![self.point1.1, self.point2.1].iter().max()
            < vec![other.point1.1, other.point2.1].iter().min()
        {
            return None;
        }

        if vec![self.point1.1, self.point2.1].iter().min()
            > vec![other.point1.1, other.point2.1].iter().max()
        {
            return None;
        }

        if self.point1.0 == self.point2.0 {
            return Some((self.point1.0, other.point1.1));
        } else {
            return Some((other.point1.0, self.point1.1));
        }
    }

    fn point_on_this(&self, point: &(i32, i32)) -> i32 {
        if self.point1.0 == self.point2.0 && self.point1.0 != point.0 {
            return (self.point2.1 - self.point1.1).abs();
        }

        if self.point1.1 == self.point2.1 && self.point1.1 != point.1 {
            return (self.point2.0 - self.point1.0).abs();
        }

        return (point.0 - self.point1.0).abs() + (point.1 - self.point1.1).abs();
    }

    fn length(&self) -> i32 {
        (self.point2.0 - self.point1.0).abs() + (self.point2.1 - self.point1.1).abs()
    }
}

// make one wire become two sets of lines, one horizontal, one vertical
fn make_lines(set: Vec<(String, i32)>) -> (Vec<Line>, Vec<Line>) {
    let (mut horizontal, mut vertical) = (vec![], vec![]);

    let mut current = (0, 0);
    for (a, b) in set {
        match a.as_str() {
            "R" => {
                let endpoint = (current.0, current.1 + b);
                horizontal.push(Line::new(current, endpoint));
                current = endpoint;
            }
            "D" => {
                let endpoint = (current.0 - b, current.1);
                vertical.push(Line::new(current, endpoint));
                current = endpoint;
            }
            "U" => {
                let endpoint = (current.0 + b, current.1);
                vertical.push(Line::new(current, endpoint));
                current = endpoint;
            }
            "L" => {
                let endpoint = (current.0, current.1 - b);
                horizontal.push(Line::new(current, endpoint));
                current = endpoint;
            }
            _ => {}
        }
    }

    (horizontal, vertical)
}

fn make_lines_in_order(set: Vec<(String, i32)>) -> Vec<Line> {
    let mut lines = vec![];
    let mut current = (0, 0);

    for (a, b) in set {
        match a.as_str() {
            "R" => {
                let endpoint = (current.0, current.1 + b);
                lines.push(Line::new(current, endpoint));
                current = endpoint;
            }
            "D" => {
                let endpoint = (current.0 - b, current.1);
                lines.push(Line::new(current, endpoint));
                current = endpoint;
            }
            "U" => {
                let endpoint = (current.0 + b, current.1);
                lines.push(Line::new(current, endpoint));
                current = endpoint;
            }
            "L" => {
                let endpoint = (current.0, current.1 - b);
                lines.push(Line::new(current, endpoint));
                current = endpoint;
            }
            _ => {}
        }
    }

    lines
}

// lines1 and lines2 cannot be same orientation
fn find_points(lines1: &Vec<Line>, lines2: &Vec<Line>) -> Vec<(i32, i32)> {
    let mut result = vec![];

    for l in lines1 {
        for l2 in lines2 {
            if let Some(p) = l.intersection_point(&l2) {
                result.push(p);
            }
        }
    }

    result
}

fn count_step_to_point(lines: &Vec<Line>, point: (i32, i32)) -> i32 {
    let mut steps = 0;
    for l in lines {
        let cache = l.point_on_this(&point);
        steps += cache;
        if cache < l.length() {
            break;
        }
    }
    steps
}

fn part2(a: &str, b: &str) -> Vec<i32> {
    let wire1 = a
        .split(",")
        .map(|x| parse_step(x))
        .collect::<Vec<(String, i32)>>();
    let wire2 = b
        .split(",")
        .map(|x| parse_step(x))
        .collect::<Vec<(String, i32)>>();

    let (wire1_lh, wire1_lv) = make_lines(wire1.clone());
    let (wire2_lh, wire2_lv) = make_lines(wire2.clone());

    let mut cache = find_points(&wire1_lh, &wire2_lv);
    cache.append(&mut find_points(&wire2_lh, &wire1_lv)); // all points

    // make order lines
    let wire1_l: Vec<Line> = make_lines_in_order(wire1);
    let wire2_l: Vec<Line> = make_lines_in_order(wire2);

    //
    let mut cache = cache
        .iter()
        .map(|&p| count_step_to_point(&wire1_l, p) + count_step_to_point(&wire2_l, p))
        .collect::<Vec<i32>>();

    cache.sort();
    cache
}

fn main() {
    //dbg!(parse_step("R75"));

    // {
    //     let l1 = Line::new((0, 0), (0, 3));
    //     let l2 = Line::new((2, 1), (-1, 1));
    //     dbg!(l1.intersection_point(&l2));
    // }

    // {
    //     let wire1 = "R8,U5,L5,D3"
    //         .split(",")
    //         .map(|x| parse_step(x))
    //         .collect::<Vec<(String, i32)>>();
    //     let wire2 = "U7,R6,D4,L4"
    //         .split(",")
    //         .map(|x| parse_step(x))
    //         .collect::<Vec<(String, i32)>>();

    //     let (wire1_lh, wire1_lv) = make_lines(wire1);
    //     let (wire2_lh, wire2_lv) = make_lines(wire2);

    //     //dbg!(find_points(&wire1_lh, &wire2_lv));
    //     //dbg!(find_points(&wire2_lh, &wire1_lv));

    //     let mut cache = find_points(&wire1_lh, &wire2_lv);
    //     cache.append(&mut find_points(&wire2_lh, &wire1_lv));

    //     let mut cache = cache.iter().map(|x| x.0 + x.1).collect::<Vec<i32>>();
    //     cache.sort();
    //     dbg!(cache[1]);
    // }

    // {
    //     let wire1 = ""
    //         .split(",")
    //         .map(|x| parse_step(x))
    //         .collect::<Vec<(String, i32)>>();
    //     let wire2 = ""
    //         .split(",")
    //         .map(|x| parse_step(x))
    //         .collect::<Vec<(String, i32)>>();

    //     let (wire1_lh, wire1_lv) = make_lines(wire1);
    //     let (wire2_lh, wire2_lv) = make_lines(wire2);

    //     //dbg!(find_points(&wire1_lh, &wire2_lv));
    //     //dbg!(find_points(&wire2_lh, &wire1_lv));

    //     let mut cache = find_points(&wire1_lh, &wire2_lv);
    //     cache.append(&mut find_points(&wire2_lh, &wire1_lv));

    //     let mut cache = cache
    //         .iter()
    //         .map(|x| x.0.abs() + x.1.abs())
    //         .collect::<Vec<i32>>();
    //     cache.sort();
    //     dbg!(cache[0]);
    // }

    dbg!(part2("", ""));
}
