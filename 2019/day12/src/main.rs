use num::integer::lcm;
use std::cell::{RefCell, RefMut};

#[derive(Debug, Copy, Clone, PartialEq)]
struct Pos {
    x: i32,
    y: i32,
    z: i32,
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Vel {
    x: i32,
    y: i32,
    z: i32,
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Moon {
    p: Pos,
    v: Vel,
    eng: i64,
}

impl Moon {
    fn new(x: i32, y: i32, z: i32) -> Self {
        Moon {
            p: Pos { x: x, y: y, z: z },
            v: Vel { x: 0, y: 0, z: 0 },
            eng: 0,
        }
    }

    // pair two moons to update their Vel
    fn pair(mut a: RefMut<Self>, mut b: RefMut<Self>) {
        if a.p.x > b.p.x {
            a.v.x -= 1;
            b.v.x += 1;
        } else if a.p.x < b.p.x {
            a.v.x += 1;
            b.v.x -= 1;
        }

        if a.p.y > b.p.y {
            a.v.y -= 1;
            b.v.y += 1;
        } else if a.p.y < b.p.y {
            a.v.y += 1;
            b.v.y -= 1;
        }

        if a.p.z > b.p.z {
            a.v.z -= 1;
            b.v.z += 1;
        } else if a.p.z < b.p.z {
            a.v.z += 1;
            b.v.z -= 1;
        }
    }

    // update pos after vel updated
    fn update_pos(&mut self) {
        self.p.x += self.v.x;
        self.p.y += self.v.y;
        self.p.z += self.v.z;
    }

    fn one_step(moons: &mut Vec<RefCell<Self>>) {
        let length = moons.len();
        for i in 0..length {
            for j in (i + 1)..length {
                Moon::pair(moons[i].borrow_mut(), moons[j].borrow_mut());
            }
        }
        for i in 0..length {
            moons[i].borrow_mut().update_pos()
        }
    }

    fn total_energy(&mut self) {
        self.eng = (self.p.x.abs() + self.p.y.abs() + self.p.z.abs()) as i64
            * (self.v.x.abs() + self.v.y.abs() + self.v.z.abs()) as i64
    }

    fn dimension_state(moons: &Vec<RefCell<Self>>, ind: usize) -> Vec<[i32; 2]> {
        match ind {
            0 => moons
                .iter()
                .map(|x| [x.borrow().p.x, x.borrow().v.x])
                .collect::<Vec<[i32; 2]>>(),
            1 => moons
                .iter()
                .map(|x| [x.borrow().p.y, x.borrow().v.y])
                .collect::<Vec<[i32; 2]>>(),
            2 => moons
                .iter()
                .map(|x| [x.borrow().p.z, x.borrow().v.z])
                .collect::<Vec<[i32; 2]>>(),
            _ => panic!(),
        }
    }
}

fn day12() {
    let mut moons = vec![
        RefCell::new(Moon::new(-4, 3, 15)),
        RefCell::new(Moon::new(-11, -10, 13)),
        RefCell::new(Moon::new(2, 2, 18)),
        RefCell::new(Moon::new(7, -1, 0)),
    ];

    for _ in 0..1000 {
        Moon::one_step(&mut moons);
    }
    moons.iter().for_each(|x| x.borrow_mut().total_energy());
    println!("after one step : {:?}", moons);
}

fn day12_part2() {
    let moons_init = vec![
        RefCell::new(Moon::new(-4, 3, 15)),
        RefCell::new(Moon::new(-11, -10, 13)),
        RefCell::new(Moon::new(2, 2, 18)),
        RefCell::new(Moon::new(7, -1, 0)),
    ];

    let mut moons = moons_init.clone();
    let mut cycle = [0; 3];
    let mut count: u64 = 0;
    loop {
        if cycle[0] != 0 && cycle[1] != 0 && cycle[2] != 0 {
            break;
        }

        Moon::one_step(&mut moons);
        count += 1;

        for i in 0..3 {
            if cycle[i] == 0
                && Moon::dimension_state(&moons, i) == Moon::dimension_state(&moons_init, i)
            {
                cycle[i] = count
            }
        }
    }

    println!("{}", lcm(cycle[0], lcm(cycle[1], cycle[2])));
}

fn main() {
    //day12()
    day12_part2();
}
