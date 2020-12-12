use tools::read_file_by_line;

enum Dir {
    E,
    W,
    N,
    S,
}

struct Ship {
    face: Dir,
    east: i32,
    west: i32,
    south: i32,
    north: i32,
}

impl Ship {
    fn turn(&mut self, d: &str, degree: i32) {
        for _ in 0..degree / 90 {
            if d == "L" {
                match self.face {
                    Dir::N => self.face = Dir::W,
                    Dir::S => self.face = Dir::E,
                    Dir::E => self.face = Dir::N,
                    Dir::W => self.face = Dir::S,
                }
            } else {
                match self.face {
                    Dir::N => self.face = Dir::E,
                    Dir::S => self.face = Dir::W,
                    Dir::E => self.face = Dir::S,
                    Dir::W => self.face = Dir::N,
                }
            }
        }
    }

    fn forward(&mut self, degree: i32) {
        match self.face {
            Dir::N => self.north += degree,
            Dir::S => self.south += degree,
            Dir::E => self.east += degree,
            Dir::W => self.west += degree,
        }
    }
}
fn parser(input: &Vec<String>) -> Vec<(String, i32)> {
    input
        .iter()
        .map(|s| {
            let (order, num) = s.split_at(1);
            (order.to_string(), num.parse().unwrap())
        })
        .collect()
}

fn handle(orders: &Vec<(String, i32)>, ship: &mut Ship) {
    for (order, num) in orders {
        match order.as_str() {
            "N" => ship.north += num,
            "S" => ship.south += num,
            "E" => ship.east += num,
            "W" => ship.west += num,
            "L" => ship.turn("L", *num),
            "R" => ship.turn("R", *num),
            "F" => ship.forward(*num),
            _ => {}
        }
    }
}

fn part1(input: &Vec<String>) -> i32 {
    let orders: Vec<(String, i32)> = parser(input);
    let mut ship = Ship {
        face: Dir::E,
        east: 0,
        west: 0,
        south: 0,
        north: 0,
    };

    handle(&orders, &mut ship);

    (ship.east - ship.west).abs() + (ship.north - ship.south).abs()
}

struct WayPoint {
    E: i32,
    W: i32,
    S: i32,
    N: i32,
}

impl WayPoint {
    fn turn(&mut self, d: &str, num: i32) {
        for _ in 0..num / 90 {
            match d {
                "L" => {
                    let e = self.E;
                    self.E = self.S;
                    self.S = self.W;
                    self.W = self.N;
                    self.N = e;
                }
                "R" => {
                    let e = self.E;
                    self.E = self.N;
                    self.N = self.W;
                    self.W = self.S;
                    self.S = e;
                }
                _ => {}
            }
        }
    }
}

fn part2(input: &Vec<String>) -> i32 {
    let orders: Vec<(String, i32)> = parser(input);
    let mut wp = WayPoint {
        E: 10,
        W: 0,
        S: 0,
        N: 1,
    };
    let mut ewsn: Vec<i32> = vec![0; 4];

    for (order, num) in orders {
        match order.as_str() {
            "N" => wp.N += num,
            "S" => wp.S += num,
            "E" => wp.E += num,
            "W" => wp.W += num,
            "L" => wp.turn("L", num),
            "R" => wp.turn("R", num),
            "F" => {
                ewsn[0] += wp.E * num;
                ewsn[1] += wp.W * num;
                ewsn[2] += wp.S * num;
                ewsn[3] += wp.N * num;
            }
            _ => {}
        }
    }

    (ewsn[0] - ewsn[1]).abs() + (ewsn[2] - ewsn[3]).abs()
}

fn main() {
    let input = read_file_by_line(String::from("./src/day12.input"));
    println!("part1: {:?}", part1(&input));
    println!("part2: {:?}", part2(&input));
}
