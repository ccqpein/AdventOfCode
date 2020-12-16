use std::collections::HashSet;
use tools::read_file_by_line;

fn part1(input: &Vec<String>) -> usize {
    let ranges = input
        .iter()
        .take(20)
        .map(|l| l.split(": "))
        .map(|mut b| {
            b.next();
            b.next().unwrap().split(" or ") //.collect::<Vec<&str>>()
        })
        //.for_each(|s| println!("{:?}", s))
        //.collect::<Vec<_>>();
        .map(|mut b| {
            vec![
                b.next().unwrap().split("-").collect::<Vec<_>>(),
                b.next().unwrap().split("-").collect::<Vec<_>>(),
            ]
        })
        .flatten()
        .collect::<Vec<_>>();
    let mut cache = vec![0; 1000];
    for r in &ranges {
        for ind in r[0].parse::<usize>().unwrap()..=r[1].parse::<usize>().unwrap() {
            cache[ind] = 1;
        }
    }

    let mut ii = input.iter();
    while ii.next().unwrap() != "nearby tickets:" {}

    let mm = ii
        .map(|l| l.split(",").collect::<Vec<_>>())
        .flatten()
        .map(|s| s.parse::<usize>().unwrap());
    //.collect::<Vec<_>>();

    mm.filter(|d| cache[*d] == 0).sum::<usize>()
    //println!("{:?}", mm);
}

fn if_ticket_legal<'a>(s: &Vec<usize>, cache: &Vec<usize>) -> bool {
    !s.iter().any(|d| cache[*d] == 0)
}

fn make_cache(input: &Vec<String>) -> Vec<usize> {
    let ranges = input
        .iter()
        .take(20)
        .map(|l| l.split(": "))
        .map(|mut b| {
            b.next();
            b.next().unwrap().split(" or ")
        })
        .map(|mut b| {
            vec![
                b.next().unwrap().split("-").collect::<Vec<_>>(),
                b.next().unwrap().split("-").collect::<Vec<_>>(),
            ]
        })
        .flatten()
        .collect::<Vec<_>>();
    let mut cache = vec![0; 1000];
    for r in &ranges {
        for ind in r[0].parse::<usize>().unwrap()..=r[1].parse::<usize>().unwrap() {
            cache[ind] = 1;
        }
    }
    cache
}

fn make_range(s: &str) -> Vec<(usize, usize)> {
    let mut d = s.split(": ");
    d.next();
    d.next()
        .unwrap()
        .split(" or ")
        .map(|w| {
            let mut b = w.split("-").map(|a| a.parse::<usize>().unwrap());
            (b.next().unwrap(), b.next().unwrap())
        })
        .collect()
}

fn part2(input: &Vec<String>) -> Vec<HashSet<usize>> {
    let cache = make_cache(input);
    //input.iter().take(22);

    let mut input_i = input.iter();
    let my_ticket = input_i.nth(22).unwrap();

    input_i.next();
    input_i.next();

    let all_nearby_tickets = input_i.map(|l| {
        l.split(",")
            .map(|w| w.parse::<usize>().unwrap())
            .collect::<Vec<usize>>()
    });

    let fileds = input
        .iter()
        .take(20)
        .map(|s| make_range(s))
        .collect::<Vec<Vec<(usize, usize)>>>();

    let mut possible_v = vec![(0..20).collect::<HashSet<usize>>(); 20];

    for t in all_nearby_tickets {
        if !if_ticket_legal(&t, &cache) {
            continue;
        }
        //println!("{:?}", t);
        for (ind, &v) in t.iter().enumerate() {
            for (f_ind, filed) in fileds.iter().enumerate() {
                if (v >= filed[0].0 && v <= filed[0].1) || (v >= filed[1].0 && v <= filed[1].1) {
                } else {
                    possible_v[f_ind].remove(&ind);
                }
            }
        }
    }

    // this result fron stdout time after time print
    for f in &[11, 4, 2, 0, 16, 14, 7, 6, 3, 9] {
        possible_v.iter_mut().for_each(|s| {
            s.remove(f);
        })
    }

    let ind_result = [16, 14, 9, 7, 6, 3];
    let mut result = 1;
    let my_ticket = my_ticket
        .split(",")
        .map(|w| w.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    for n in ind_result.iter() {
        result *= my_ticket[*n as usize]
    }

    println!("finally: {}", result);
    possible_v
}

fn main() {
    let input = read_file_by_line("./src/day16.input");

    //println!("part1: {:?}", part1(&input));
    // println!(
    //     "make range: {:?}",
    //     make_range("departure location: 36-269 or 275-973")
    // );

    //println!("{:?}", input[22]);
    for (i, v) in part2(&input).iter().enumerate() {
        println!("i: {}, v: {:?}", i, v);
    }
}
