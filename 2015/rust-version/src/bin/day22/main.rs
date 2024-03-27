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
                    *n -= 1
                } else {
                    p.armor -= 7;
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
) -> Option<usize> {
    if let Some(d) = cache.get(&(p.clone(), b.clone(), status.clone(), true)) {
        return *d;
    }

    status_settle(&mut p, &mut b, &mut status);

    if p.hp <= 0 {
        return None;
    }

    if b.hp <= 0 {
        return Some(0);
    }

    let can_cast = status
        .iter()
        .enumerate()
        .filter(|(ind, s)| s.zerop())
        .map(|(ind, _)| ind)
        .collect::<Vec<_>>();

    let result = (0..5)
        .filter_map(|i| {
            let mut ss = status.clone();
            let mut pp = p.clone();
            let mut bb = b.clone();
            if can_cast.contains(&i) {
                match ss.get(i).unwrap() {
                    Status::Shield(_) if p.mana >= 113 => {
                        ss.get_mut(i).unwrap().recharge();
                        pp.mana -= 113;
                        if let Some(d) = boss_turn(pp, bb, ss, cache) {
                            Some(d + 113)
                        } else {
                            None
                        }
                    }
                    Status::Poison(_) if p.mana >= 173 => {
                        ss.get_mut(i).unwrap().recharge();
                        pp.mana -= 173;
                        if let Some(d) = boss_turn(pp, bb, ss, cache) {
                            Some(d + 173)
                        } else {
                            None
                        }
                    }
                    Status::Recharge(_) if p.mana >= 229 => {
                        ss.get_mut(i).unwrap().recharge();
                        pp.mana -= 229;
                        if let Some(d) = boss_turn(pp, bb, ss, cache) {
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
                    3 => {
                        pp.mana -= 53;
                        bb.hp -= 4;
                        if let Some(d) = boss_turn(pp, bb, ss, cache) {
                            Some(d + 53)
                        } else {
                            None
                        }
                    }
                    // drain
                    4 => {
                        pp.mana -= 73;
                        pp.hp += 2;
                        bb.hp -= 2;
                        if let Some(d) = boss_turn(pp, bb, ss, cache) {
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
) -> Option<usize> {
    if let Some(d) = cache.get(&(p.clone(), b.clone(), status.clone(), false)) {
        return *d;
    }

    status_settle(&mut p, &mut b, &mut status);

    if b.hp <= 0 {
        return Some(0);
    }

    let result = if b.damage <= p.armor {
        p.hp -= 1;
        player_turn(p.clone(), b.clone(), status.clone(), cache)
    } else {
        p.hp -= b.damage - p.armor;
        player_turn(p.clone(), b.clone(), status.clone(), cache)
    };

    cache.insert((p, b, status.clone(), false), result);
    result
}

fn handle(p: &mut Player, b: &mut Boss, cache: HashMap<(Player, Boss, Vec<Status>), usize>) {}

fn day22(p: Player, b: Boss) {}

fn main() {
    let p = Player {
        hp: 1,
        mana: 167,
        armor: 0,
    };
    let b = Boss { hp: 9, damage: 8 };
    let status = vec![Status::Shield(1), Status::Poison(5), Status::Recharge(0)];
    let mut cache = HashMap::new();
    player_turn(p, b, status, &mut cache);

    cache.iter().for_each(|x| println!("{:?}", x));
}
