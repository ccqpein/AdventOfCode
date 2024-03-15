use regex::Regex;
use tools::custom_carrier::*;
use tools::*;

// fn check_bkp(s: &[AlphaBetLowCase]) -> bool {
//     //let mut s = s.iter();
//     let mut pair = 0;
//     let mut three = 0;

//     let mut a = *s[0].byte();
//     for c in &s[1..] {
//         if *c.byte() == a {
//             pair += 1;
//             a = 0;
//             if three < 2 {
//                 three = 0;
//             }
//             continue;
//         }

//         if *c.byte() == a + 1 {
//             three += 1;
//         } else {
//             if three == 1 {
//                 three = 0
//             }
//         }

//         a = *c.byte();
//     }

//     dbg!(&pair);
//     dbg!(&three);
//     pair == 2 && three >= 2
// }

// fn check(s: &[AlphaBetLowCase]) -> bool {
//     dbg!(&String::from_utf8(s.iter().map(|b| b.byte().clone()).collect()).unwrap());
//     let re = Regex::new(r"(.)\1").unwrap();
//     for r in re
//         .captures(&String::from_utf8(s.iter().map(|b| b.byte().clone()).collect()).unwrap())
//         .unwrap()
//         .iter()
//     {
//         dbg!(r.unwrap().as_str());
//     }

//     false
// }

fn check_pair(s: &[AlphaBetLowCase]) -> bool {
    let mut pair = 0;
    let mut a = *s[0].byte();
    for c in &s[1..] {
        if *c.byte() == a {
            pair += 1;
            a = 0;
            continue;
        }
        a = *c.byte();
    }

    pair >= 2
}
fn check_three(s: &[AlphaBetLowCase]) -> bool {
    let mut three = 0;
    let mut a = *s[0].byte();

    for c in &s[1..] {
        if *c.byte() == a + 1 {
            three += 1;
        } else {
            if three == 1 {
                three = 0
            }
        }

        if three == 2 {
            return true;
        }
        a = *c.byte();
    }

    false
}

fn check(s: &[AlphaBetLowCase]) -> bool {
    //dbg!(check_pair(s));
    //dbg!(check_three(s));
    check_pair(s) && check_three(s)
}

fn delete_iol(s: &mut [AlphaBetLowCase]) {
    for i in 0..s.len() {
        match s.get_mut(i).unwrap().byte() {
            105 | 111 | 108 => {
                s.get_mut(i).unwrap().next_mut();
                for j in i + 1..s.len() {
                    *s.get_mut(j).unwrap() = AlphaBetLowCase::try_from(97_u8).unwrap();
                }
            }
            _ => (),
        }
    }
}

fn day11(input: &str) -> String {
    let input: Vec<AlphaBetLowCase> = input
        .bytes()
        .map(|b| b.try_into().unwrap())
        .collect::<Vec<_>>();

    let mut input = input.into_align(Orientation::ToLeft).unwrap();
    loop {
        //dbg!(String::from_utf8(input.inner().iter().map(|b| b.byte().clone()).collect(),).unwrap());
        input.next();
        delete_iol(input.inner_mut());
        if check(&input.inner()) {
            return String::from_utf8(
                input
                    .inner()
                    .into_iter()
                    .map(|b| b.byte().clone())
                    .collect(),
            )
            .unwrap();
        }
    }
}

fn main() {
    let input = "hxbxwxba";
    //let input = "abcdefgh";
    //let input = "ghijklmn";

    println!("1: {:?}", day11(input));
    println!("2: {:?}", day11(&day11(input)));

    // assert!(!check(
    //     "hijklmmn"
    //         .bytes()
    //         .map(|b| b.try_into().unwrap())
    //         .collect::<Vec<AlphaBetLowCase>>()
    //         .into_align(Orientation::ToLeft)
    //         .unwrap()
    //         .inner()
    // ));

    // assert!(check(
    //     "abcdffaa"
    //         .bytes()
    //         .map(|b| b.try_into().unwrap())
    //         .collect::<Vec<AlphaBetLowCase>>()
    //         .into_align(Orientation::ToLeft)
    //         .unwrap()
    //         .inner()
    // ));
    // assert!(check(
    //     "ghjaabcc"
    //         .bytes()
    //         .map(|b| b.try_into().unwrap())
    //         .collect::<Vec<AlphaBetLowCase>>()
    //         .into_align(Orientation::ToLeft)
    //         .unwrap()
    //         .inner()
    // ));

    // assert!(!check(
    //     "abcdeggg"
    //         .bytes()
    //         .map(|b| b.try_into().unwrap())
    //         .collect::<Vec<AlphaBetLowCase>>()
    //         .into_align(Orientation::ToLeft)
    //         .unwrap()
    //         .inner()
    // ));
}
