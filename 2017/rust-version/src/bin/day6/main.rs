use std::collections::{HashMap, HashSet};


fn max_v_ind(input: &[i32]) -> usize {
    let vv = input.iter().max().unwrap();

    input.iter().position(|&v| v == *vv).unwrap()
}

fn cycle_give(input: &mut [i32], ind: usize) {
    let mut vv = input.get(ind).unwrap().clone();
    *input.get_mut(ind).unwrap() = 0;
    loop {
        for i in ind + 1..input.len() {
            *input.get_mut(i).unwrap() += 1;
            vv -= 1;
            if vv == 0 {
                return;
            }
        }

        for i in 0..=ind {
            *input.get_mut(i).unwrap() += 1;
            vv -= 1;
            if vv == 0 {
                return;
            }
        }
    }
}

fn day6(input: &mut [i32]) -> i32 {
    let mut cache = HashSet::new();

    cache.insert(input.to_vec());

    let mut step = 0;

    loop {
        let ind = max_v_ind(input);

        cycle_give(input, ind);
        step += 1;

        if cache.get(&input.to_vec()).is_some() {
            return step;
        } else {
            cache.insert(input.to_vec());
        }
    }
}

fn day6_2(input: &mut [i32]) -> i32 {
    let mut cache = HashMap::new();

    cache.insert(input.to_vec(), 0);

    let mut step = 0;
    //let mut flag = false;

    loop {
        let ind = max_v_ind(input);
        dbg!(ind);

        cycle_give(input, ind);
        step += 1;

        //dbg!(&input);
        if let Some(vv) = cache.get(&input.to_vec()) {
            return step - vv;
        } else {
            cache.insert(input.to_vec(), step);
        }
    }
}

fn main() {
    let mut input = vec![0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11];
    //let mut input = vec![0, 2, 7, 0];

    dbg!(day6(&mut input));

    dbg!(day6_2(&mut input));
}
