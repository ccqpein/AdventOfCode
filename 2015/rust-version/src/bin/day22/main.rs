use std::collections::HashMap;

use tools::*;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
struct Player {
    hp: i32,
    mana: i32,
    armor: i32,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
struct Boss {
    hp: i32,
    damage: i32,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
enum Status {
    Shield(usize),
    Poison(usize),
    Recharge(usize),
    MM,
    Drain,
}

impl Status {
    fn zerop(&self) -> bool {
        match self {
            Status::Shield(d) if *d == 0 => true,
            Status::Poison(d) if *d == 0 => true,
            Status::Recharge(d) if *d == 0 => true,
            _ => false,
        }
    }

    fn recharge(&mut self) {
        match self {
            Status::Shield(d) => *d = 6,
            Status::Poison(d) => *d = 6,
            Status::Recharge(d) => *d = 5,
            _ => (),
        }
    }
}

fn status_settle(p: &mut Player, b: &mut Boss, status: &mut Vec<Status>) {
    //vec![Shield, Poison, Recharge]
    for s in status.iter_mut() {
        match s {
            Status::Shield(n) => {
                if *n != 0 {
                    *n -= 1;
                    p.armor = 7;
                } else {
                    p.armor = 0;
                }
            }
            Status::Poison(n) => {
                if *n != 0 {
                    b.hp -= 3;
                    *n -= 1;
                }
            }
            Status::Recharge(n) => {
                if *n != 0 {
                    p.mana += 101;
                    *n -= 1
                }
            }
            _ => (),
        }
    }
}

fn player_turn(
    mut p: Player,
    mut b: Boss,
    mut status: Vec<Status>,
    cache: &mut HashMap<(Player, Boss, Vec<Status>, bool), Option<usize>>,
    p2: bool,
) -> Option<usize> {
    if let Some(d) = cache.get(&(p.clone(), b.clone(), status.clone(), true)) {
        return *d;
    }

    let mut ss = status.clone();
    let mut pp = p.clone();
    let mut bb = b.clone();

    if p2 {
        pp.hp -= 1;
        if pp.hp <= 0 {
            return None;
        }
    }

    status_settle(&mut pp, &mut bb, &mut ss);

    if pp.hp <= 0 {
        return None;
    }

    if bb.hp <= 0 {
        return Some(0);
    }

    let can_cast = ss
        .iter()
        .enumerate()
        .filter(|(ind, s)| s.zerop())
        .map(|(ind, _)| ind)
        .collect::<Vec<_>>();

    let result = (0..5)
        .filter_map(|i| {
            let mut sss = ss.clone();
            let mut ppp = pp.clone();
            let mut bbb = bb.clone();
            if can_cast.contains(&i) {
                match sss.get(i).unwrap() {
                    Status::Shield(_) if ppp.mana >= 113 => {
                        sss.get_mut(i).unwrap().recharge();
                        ppp.mana -= 113;
                        if let Some(d) = boss_turn(ppp, bbb, sss, cache, p2) {
                            Some(d + 113)
                        } else {
                            None
                        }
                    }
                    Status::Poison(_) if ppp.mana >= 173 => {
                        sss.get_mut(i).unwrap().recharge();
                        ppp.mana -= 173;
                        if let Some(d) = boss_turn(ppp, bbb, sss, cache, p2) {
                            Some(d + 173)
                        } else {
                            None
                        }
                    }
                    Status::Recharge(_) if ppp.mana >= 229 => {
                        sss.get_mut(i).unwrap().recharge();
                        ppp.mana -= 229;
                        if let Some(d) = boss_turn(ppp, bbb, sss, cache, p2) {
                            Some(d + 229)
                        } else {
                            None
                        }
                    }
                    //Status::MM => todo!(),
                    //Status::Drain => todo!(),
                    _ => None,
                }
            } else {
                match i {
                    // mm
                    3 if ppp.mana >= 53 => {
                        ppp.mana -= 53;
                        bbb.hp -= 4;
                        if let Some(d) = boss_turn(ppp, bbb, sss, cache, p2) {
                            Some(d + 53)
                        } else {
                            None
                        }
                    }
                    // drain
                    4 if ppp.mana >= 73 => {
                        ppp.mana -= 73;
                        ppp.hp += 2;
                        bbb.hp -= 2;
                        if let Some(d) = boss_turn(ppp, bbb, sss, cache, p2) {
                            Some(d + 73)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        })
        .min();
    cache.insert((p, b, status.clone(), true), result);
    result
}

fn boss_turn(
    mut p: Player,
    mut b: Boss,
    mut status: Vec<Status>,
    cache: &mut HashMap<(Player, Boss, Vec<Status>, bool), Option<usize>>,
    p2: bool,
) -> Option<usize> {
    if let Some(d) = cache.get(&(p.clone(), b.clone(), status.clone(), false)) {
        return *d;
    }

    let mut ss = status.clone();
    let mut pp = p.clone();
    let mut bb = b.clone();

    status_settle(&mut pp, &mut bb, &mut ss);

    if bb.hp <= 0 {
        return Some(0);
    }

    let result = if bb.damage <= pp.armor {
        pp.hp -= 1;
        player_turn(pp, bb, ss, cache, p2)
    } else {
        pp.hp -= bb.damage - pp.armor;
        player_turn(pp, bb, ss, cache, p2)
    };

    cache.insert((p, b, status.clone(), false), result);
    result
}

fn main() {
    let p = Player {
        hp: 50,
        mana: 500,
        armor: 0,
    };
    let b = Boss { hp: 58, damage: 9 };
    let status = vec![Status::Shield(0), Status::Poison(0), Status::Recharge(0)];

    let mut cache = HashMap::new();
    println!(
        "1: {:?}",
        player_turn(p.clone(), b.clone(), status.clone(), &mut cache, false)
    );

    let mut cache = HashMap::new();
    println!("2: {:?}", player_turn(p, b, status, &mut cache, true));

    //cache.iter().for_each(|x| println!("{:?}", x));
    // println!(
    //     "cache: {:?}",
    //     cache
    //         .iter()
    //         .filter_map(|(k, v)| if k.3 { *v } else { None })
    //         .min()
    // );
}
