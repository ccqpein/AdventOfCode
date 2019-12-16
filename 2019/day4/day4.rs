fn judge_this_number(input: &i32) -> bool {
    let mut modu = 100000;
    let mut double_flag = false;

    let (mut a, mut b) = (input / modu, input % modu);
    while modu != 1 {
        //dbg!((a, b));
        modu /= 10;
        if b / modu < a {
            return false;
        }
        if b / modu == a {
            double_flag = true
        }

        a = b / modu;
        b = b % modu;
    }

    double_flag
}

fn day4(r1: i32, r2: i32) -> usize {
    (r1..=r2).filter(|&x| judge_this_number(&x)).count()
}

fn judge_this_number_part2(input: &i32) -> bool {
    use std::collections::HashMap;
    let mut modu = 100000;
    let mut double_flag = false;
    let mut temp: HashMap<i32, i32> = HashMap::new();

    let (mut a, mut b) = (input / modu, input % modu);
    while modu != 1 {
        modu /= 10;
        if b / modu < a {
            return false;
        }

        if b / modu == a {
            *temp.entry(a).or_insert(1) += 1;
        }

        a = b / modu;
        b = b % modu;
    }
    //dbg!(&temp);
    temp.iter().filter(|(k, v)| **v == 2).count() >= 1
}

fn day4_part2(r1: i32, r2: i32) -> usize {
    (r1..=r2).filter(|&x| judge_this_number_part2(&x)).count()
}

fn main() {
    //dbg!(day4(range1, range2));
    //dbg!(judge_this_number(&123789));

    //dbg!(day4(273025, 767253));

    //dbg!(judge_this_number_part2(&111122));
    //dbg!(judge_this_number_part2(&123444));
    //dbg!(judge_this_number_part2(&222455));

    dbg!(day4_part2(273025, 767253));
}
