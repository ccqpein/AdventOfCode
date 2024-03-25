use tools::*;

/// cost,a
#[derive(Debug)]
struct Weapon(usize, usize);
/// cost,d
#[derive(Debug)]
struct Armor(usize, usize);
/// cost,a,d
#[derive(Clone, Copy, Debug)]
struct Ring(usize, usize, usize);

/// a, d, cost
#[derive(Debug)]
struct Status(usize, usize, usize);

impl Status {
    fn new(w: &Weapon, a: &Armor, r: &[Ring]) -> Self {
        Self(
            w.1 + r.iter().map(|rr| rr.1).sum::<usize>(),
            a.1 + r.iter().map(|rr| rr.2).sum::<usize>(),
            w.0 + a.0 + r.iter().map(|rr| rr.0).sum::<usize>(),
        )
    }
}

fn check(myhp: f64, me: &mut Status, enhp: f64, en: &mut Status) -> bool {
    if me.0 <= en.1 {
        me.0 = en.1 + 1;
    }

    if en.0 <= me.1 {
        en.0 = me.1 + 1
    }

    (enhp / ((me.0 - en.1) as f64)).ceil() <= (myhp / ((en.0 - me.1) as f64)).ceil()
}

fn day21(hp: f64, d: usize, att: usize, ws: &[Weapon], ars: &[Armor], rs: &[Ring]) -> usize {
    let mut result = vec![];

    for w in ws {
        for a in ars {
            for i in 0..rs.len() {
                for j in 0..rs.len() {
                    if i == j && i != 0 {
                        continue;
                    }
                    let mut s = Status::new(w, a, &vec![rs[i], rs[j]]);
                    let mut en_s = Status(att, d, 0);

                    if check(100.0, &mut s, hp, &mut en_s) {
                        // println!(
                        //     "s: {:?}, w: {:?}, a:{:?}, r1: {:?}, r2: {:?}",
                        //     s, w, a, rs[i], rs[j]
                        // );
                        result.push(s.2)
                    }
                }
            }
        }
    }
    *result.iter().min().unwrap()
}

fn day21_2(hp: f64, d: usize, att: usize, ws: &[Weapon], ars: &[Armor], rs: &[Ring]) -> usize {
    let mut result = vec![];

    for w in ws {
        for a in ars {
            for i in 0..rs.len() {
                for j in 0..rs.len() {
                    if i == j && i != 0 {
                        continue;
                    }
                    let mut s = Status::new(w, a, &vec![rs[i], rs[j]]);
                    let mut en_s = Status(att, d, 0);

                    if !check(100.0, &mut s, hp, &mut en_s) {
                        result.push(s.2)
                    }
                }
            }
        }
    }
    *result.iter().max().unwrap()
}

fn main() {
    let weapons = vec![
        Weapon(8, 4),
        Weapon(10, 5),
        Weapon(25, 6),
        Weapon(40, 7),
        Weapon(74, 8),
    ];

    let armors = vec![
        Armor(0, 0),
        Armor(13, 1),
        Armor(31, 2),
        Armor(53, 3),
        Armor(75, 4),
        Armor(102, 5),
    ];
    let rings = vec![
        Ring(0, 0, 0),
        Ring(25, 1, 0),
        Ring(50, 2, 0),
        Ring(100, 3, 0),
        Ring(20, 0, 1),
        Ring(40, 0, 2),
        Ring(80, 0, 3),
    ];

    // assert!(check(8.0, &mut Status(5, 5, 0), 12.0, &mut Status(7, 2, 0)));
    // assert!(check(
    //     100.0,
    //     &mut Status(7, 5, 0),
    //     103.0,
    //     &mut Status(9, 2, 0)
    // ));
    // assert!(check(
    //     100.0,
    //     &mut Status(8, 4, 0),
    //     103.0,
    //     &mut Status(9, 2, 0)
    // ));

    println!("1: {}", day21(103.0, 2, 9, &weapons, &armors, &rings));
    println!("2: {}", day21_2(103.0, 2, 9, &weapons, &armors, &rings));
}
