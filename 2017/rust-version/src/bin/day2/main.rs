use tools::*;

fn day2(inputs: &[String]) -> i32 {
    let inputs = inputs.into_iter().map(|l| {
        l.split("\t")
            .filter(|s| !s.is_empty())
            .map(|s| {
                //dbg!(s);
                s.parse::<i32>().unwrap()
            })
            .collect::<Vec<_>>()
    });
    //.collect::<Vec<Vec<i32>>>();

    inputs
        .into_iter()
        .map(|mut l| {
            l.sort();
            l.last().unwrap() - l.first().unwrap()
        })
        .sum()
}

fn find_numbers_divide(line: &mut [i32]) -> (i32, i32) {
    line.sort();
    for i in (0..line.len()).rev() {
        for j in 0..line.len() {
            if line[i] % line[j] == 0 && line[i] != line[j] {
                return (line[i], line[j]);
            }
        }
    }
    (0, 0)
}

fn day2_2(inputs: &[String]) -> i32 {
    let inputs = inputs.into_iter().map(|l| {
        l.split("\t")
            .filter(|s| !s.is_empty())
            .map(|s| {
                //dbg!(s);
                s.parse::<i32>().unwrap()
            })
            .collect::<Vec<_>>()
    });

    inputs
        .into_iter()
        .map(|mut l| {
            l.sort();
            let (a, b) = find_numbers_divide(&mut l);
            //dbg!(a);
            //dbg!(b);
            a / b
        })
        .sum()
}

fn main() {
    let input = read_file_by_line("../inputs/day2.input");
    dbg!(day2(&input));

    // let input = vec![
    //     "5\t9\t2\t8".to_string(),
    //     "9\t4\t7\t3".to_string(),
    //     "3\t8\t6\t5".to_string(),
    // ];
    dbg!(day2_2(&input));
}
