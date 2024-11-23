use tools::*;

fn one_step(list: &mut [i32], ind: i32) -> Option<i32> {
    if ind >= list.len() as i32 || ind < 0 {
        return None;
    }

    let v = list.get(ind as usize).cloned().unwrap();

    *list.get_mut(ind as usize).unwrap() += 1;
    Some(v + ind)
}

fn day5(input: &[String]) -> i32 {
    let mut input = input
        .into_iter()
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();

    let mut result = 0;
    let mut ind = 0;
    while let Some(next_ind) = one_step(&mut input, ind) {
        ind = next_ind;
        result += 1
    }

    result
}

fn one_step_2(list: &mut [i32], ind: i32) -> Option<i32> {
    if ind >= list.len() as i32 || ind < 0 {
        return None;
    }

    let v = list.get(ind as usize).cloned().unwrap();

    if v < 3 {
        *list.get_mut(ind as usize).unwrap() += 1;
    } else {
        *list.get_mut(ind as usize).unwrap() -= 1;
    }

    Some(v + ind)
}

fn day5_2(input: &[String]) -> i32 {
    let mut input = input
        .into_iter()
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<i32>>();

    let mut result = 0;
    let mut ind = 0;
    while let Some(next_ind) = one_step_2(&mut input, ind) {
        ind = next_ind;
        result += 1
    }
    //dbg!(input);
    result
}

fn main() {
    let input = read_file_by_line("../inputs/day5.input");
    // let input: Vec<String> = vec!["0", "3", "0", "1", "-3"]
    //     .into_iter()
    //     .map(|s| s.to_string())
    //     .collect();

    dbg!(day5(&input));
    dbg!(day5_2(&input));
}
