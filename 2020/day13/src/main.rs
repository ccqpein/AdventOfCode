use tools::read_file_by_line;

fn part1(input: &Vec<String>) -> Vec<(i32, i32)> {
    let target = input[0].parse::<f32>().unwrap();
    let all_bus = input[1]
        .split(',')
        .filter_map(|s| {
            if let Ok(d) = s.parse::<f32>() {
                Some(d)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    all_bus
        .iter()
        .map(|d| {
            let x = target / d;
            if x != 0_f32 {
                (((x.floor() + 1_f32) * d - target) as i32, *d as i32)
            } else {
                (0_i32, *d as i32)
            }
        })
        .collect()
}

fn find_largest(input: &String) -> Vec<(i64, i64)> {
    let mut cinput = input.split(',').collect::<Vec<_>>();

    let mut all_bus = vec![];
    //let mut all_inter = vec![];
    //let mut count = 0;
    for c in &cinput {
        if let Ok(d) = c.parse::<i64>() {
            //all_inter.push(count);
            //count = 0;
            all_bus.push(d);
        }
        //count += 1
    }

    //println!("{:?}", all_bus);
    //println!("{:?}", all_inter);
    //println!("{:?}", &cinput);

    let mut sortedbus = all_bus.clone();
    sortedbus.sort();

    let mut last_ind = cinput
        .iter()
        .position(|&x| x == &format!("{}", sortedbus.last().unwrap()))
        .unwrap();

    let mut result = vec![];

    for bus in sortedbus.iter().rev() {
        //println!("{:?}", bus);
        let bus_ind = cinput
            .iter()
            .position(|x| {
                if let Ok(d) = x.parse::<i64>() {
                    d == *bus
                } else {
                    false
                }
            })
            .unwrap();

        let count = bus_ind as i64 - last_ind as i64;

        result.push((*bus, count));
        last_ind = bus_ind
    }

    result
}

fn part2(input: &Vec<String>) -> i64 {
    let largest_pair = find_largest(&input[1]);
    let mut flag = true;
    for count in 1.. {
        if !flag {
            println!("largest: {}", largest_pair[0].0);
            return (count - 1) * largest_pair[0].0;
        }
        let mut cache = largest_pair[0].0 * count;
        for ind in 1..largest_pair.len() {
            if (cache + largest_pair[ind].1) % largest_pair[ind].0 == 0 {
                cache += largest_pair[ind].1;
                continue;
            } else {
                flag = !flag;
                break;
            }
        }
        flag = !flag
    }
    0
}

fn part2_v2(input: &Vec<String>) -> i128 {
    let cinput = input[1].split(',').collect::<Vec<_>>();
    let mut pos = vec![];
    let mut buses = vec![];

    let mut offset = 0;
    for c in cinput {
        if c == "x" {
            offset += 1
        } else {
            pos.push(offset);
            buses.push(c.parse::<i128>().unwrap());
            offset += 1
        }
    }

    //println!("{:?}", buses);
    //println!("{:?}", pos);

    let p = buses.iter().product::<i128>();
    //println!("big: {}", p);

    let mut sum = 0;
    for (ni, ai) in buses.iter().zip(pos) {
        println!("step: {}", (-ai) * mod_inv(p / ni, *ni) * p / ni);
        sum += (-ai) * mod_inv(p / ni, *ni) * p / ni;
    }

    //println!("{}", sum);
    sum.rem_euclid(p)
}

fn mod_inv(a: i128, module: i128) -> i128 {
    let mut mn = (module, a);
    let mut xy = (0, 1);

    while mn.1 != 0 {
        xy = (xy.1, xy.0 - (mn.0 / mn.1) * xy.1);
        mn = (mn.1, mn.0 % mn.1);
    }

    while xy.0 < 0 {
        xy.0 += module;
    }
    xy.0
}

fn main() {
    let input = read_file_by_line("./src/day13.input");
    println!("part1: {:?}", part1(&input));
    //println!("{:?}", find_largest(&input[1]));
    //println!("part2: {:?}", part2(&input));
    println!("part2: {:?}", part2_v2(&input));
}
