fn count_next_ind(current: usize, len: usize, num: usize) -> usize {
    (current + num) % len + 1
}

fn day17(times: usize, num: usize) {
    let mut stack = vec![0]; // yep, it is slow
    let mut current_ind = 0;
    for i in 1..=times {
        //println!("{:?}", stack);
        let next_ind = count_next_ind(current_ind, stack.len(), num);
        let (head, tail) = stack.split_at_mut(next_ind);

        let mut head = head.to_vec();
        let mut tail = tail.to_vec();

        head.push(i as i32);
        head.append(&mut tail);
        stack = head.to_vec();
        current_ind = next_ind
    }
    println!(
        "ind: {}, around: {:?}",
        current_ind,
        &stack[current_ind - 3..current_ind + 4]
    );
}

fn day17_2(times: usize, num: usize) {
    let mut stack = vec![0]; // yep, it is slow
    let mut current_ind = 0;
    let mut len = stack.len();
    for i in 1..=times {
        //println!("{:?}", stack);
        let next_ind = count_next_ind(current_ind, len, num);
        if next_ind == 1 {
            println!("time {}", i);
        }

        len += 1;
        current_ind = next_ind
    }
    println!("ind: {}", current_ind,);
}

fn main() {
    dbg!(count_next_ind(0, 1, 3));
    dbg!(count_next_ind(1, 2, 3));
    dbg!(count_next_ind(1, 3, 3));

    //day17(2017, 3);
    //day17(2017, 367);
    day17_2(50000000, 367);
}
