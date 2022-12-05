use tools::*;

fn day5(input: &Vec<String>, part: i32) -> String {
    let mut stack: Vec<Vec<String>> = vec![
        vec!["Q", "H", "C", "T", "N", "S", "V", "B"],
        vec!["G", "B", "D", "W"],
        vec!["B", "Q", "S", "T", "R", "W", "F"],
        vec!["N", "D", "J", "Z", "S", "W", "G", "L"],
        vec!["F", "V", "D", "P", "M"],
        vec!["J", "W", "F"],
        vec!["V", "J", "B", "Q", "N", "L"],
        vec!["N", "S", "Q", "J", "C", "R", "T", "G"],
        vec!["M", "D", "W", "C", "Q", "S", "J"],
    ]
    .into_iter()
    .map(|a| a.into_iter().map(|s| s.to_string()).collect())
    .collect();

    for line in input {
        let mut commands = line.split(' ');
        commands.next();
        let num = commands.next().unwrap().parse::<usize>().unwrap();
        commands.next();
        let ff = commands.next().unwrap().parse::<usize>().unwrap() - 1;
        commands.next();
        let tt = commands.next().unwrap().parse::<usize>().unwrap() - 1;

        stack[tt] = {
            //let mut head = stack[ff][0..num].to_vec();
            let mut head = stack[ff].drain(0..num).collect::<Vec<_>>();
            if part == 1 {
                head.reverse();
            }
            head.append(&mut stack[tt]);
            head
        };

        //stack[ff] = stack[ff][num..].to_vec();
        //stack[ff].drain(0..num);
    }
    String::from_iter(stack.iter().map(|l| l[0].clone()))
}

fn main() {
    //let input = read_file_by_line("./inputs/day5_demo.input")
    let input = read_file_by_line("./inputs/day5.input");
    println!("{}", day5(&input, 1));
    println!("{}", day5(&input, 2));
}
