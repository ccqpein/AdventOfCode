use std::{collections::HashMap, time::Duration};

use regex::Regex;
use tokio::{
    select,
    sync::mpsc::{self, *},
    time,
};
use tools::read_file_by_line;

fn parse_inputs(inputs: &[String]) -> Vec<(String, String, Option<String>)> {
    let re = Regex::new(r"^(\w+)\s+(\d+|[a-z])(\s+(-?\d+|[a-z]))?$").unwrap();

    let mut parsed_input = vec![];
    for l in inputs {
        dbg!(l);
        let Some(c) = re.captures(l) else { panic!() };
        parsed_input.push((
            c[1].to_string(),
            c[2].to_string(),
            c.get(4).map(|v| v.as_str().to_string()),
        ));
    }

    //dbg!(parsed_input);
    parsed_input
}

fn day18(inputs: &[String]) {
    let parsed_input = parse_inputs(inputs);
    let mut ind = 0;
    let mut memory = HashMap::new();
    let mut last = 0;
    loop {
        let Some((a, b, c)) = parsed_input.get(ind as usize) else {
            break;
        };
        println!("{:?}", memory);
        println!("{a} {b} {:?}", c);

        match (a.as_str(), b.as_str(), c.as_ref().map(|v| v.as_str())) {
            ("snd", _, None) => {
                last = *memory.entry(b).or_insert(0);
            }
            ("set", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                *memory.entry(b).or_insert(0) = digt
            }
            ("add", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                *memory.entry(b).or_insert(0) += digt
            }
            ("mul", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                *memory.entry(b).or_insert(0) *= digt
            }
            ("mod", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(&c.as_ref().unwrap()).or_insert(0),
                };

                *memory.entry(b).or_insert(0) %= digt;
            }
            ("rcv", _, _) => {
                let en = memory.entry(b).or_insert(0);
                if *en != 0 {
                    break;
                }
            }
            ("jgz", _, _) => {
                let digt = match b.parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(b).or_insert(0),
                };

                if digt > 0 {
                    ind += c.as_ref().unwrap().parse::<i64>().unwrap();
                    continue;
                }
            }
            _ => unreachable!(),
        }

        ind += 1
    }
    println!("table: {:?}", memory);
    println!("last: {last}")
}

///////////////////////////////////////////////
//////////////////////////////////////////////

async fn one_program(
    input: Vec<(String, String, Option<String>)>,
    id: i64,
    snd: Sender<i64>,
    mut rev: Receiver<i64>,
) {
    let mut ind = 0;
    let mut memory = HashMap::new();
    memory.insert("p".to_string(), id);

    loop {
        let Some((a, b, c)) = input.get(ind as usize) else {
            break;
        };

        match (a.as_str(), b.as_str(), c.as_ref().map(|v| v.as_str())) {
            ("snd", _, None) => {
                let v = *memory.entry(b.to_string()).or_insert(0);
                snd.send(v).await;
            }
            ("set", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(c.as_ref().unwrap().to_string()).or_insert(0),
                };

                *memory.entry(b.to_string()).or_insert(0) = digt
            }
            ("add", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(c.as_ref().unwrap().to_string()).or_insert(0),
                };

                *memory.entry(b.to_string()).or_insert(0) += digt
            }
            ("mul", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(c.as_ref().unwrap().to_string()).or_insert(0),
                };

                *memory.entry(b.to_string()).or_insert(0) *= digt
            }
            ("mod", _, _) => {
                let digt = match c.as_ref().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(c.as_ref().unwrap().to_string()).or_insert(0),
                };

                *memory.entry(b.to_string()).or_insert(0) %= digt;
            }
            ("rcv", _, _) => {
                let v = rev.recv().await.unwrap();
                memory.insert(b.to_string(), v);
            }
            ("jgz", _, _) => {
                let digt = match b.parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(b.to_string()).or_insert(0),
                };

                let jmp_step = match c.clone().unwrap().parse::<i64>() {
                    Ok(d) => d,
                    Err(_) => *memory.entry(c.clone().unwrap().to_string()).or_insert(0),
                };

                if digt > 0 {
                    ind += jmp_step;
                    continue;
                }
            }
            _ => unreachable!(),
        }

        ind += 1
    }
}

async fn day18_p2(inputs: &[String]) {
    let (tx0, mut rx0) = mpsc::channel(10000);
    let (tx01, mut rx01) = mpsc::channel(10000);

    let (tx1, mut rx1) = mpsc::channel(10000);
    let (tx11, mut rx11) = mpsc::channel(10000);

    let p_inputs = parse_inputs(inputs);

    tokio::spawn(one_program(p_inputs.clone(), 0, tx1, rx01));
    tokio::spawn(one_program(p_inputs.clone(), 1, tx0, rx11));

    // stop flag design
    let mut interval = time::interval(Duration::from_secs(1));
    let mut stop_flag = false;

    let mut send_from_1 = 0;
    let mut send_from_0 = 0;
    loop {
        select! {
            Some(v0) = rx0.recv() => {
                send_from_1 +=1;
                tx01.send(v0).await;
                stop_flag = false;
            }
            Some(v1) = rx1.recv() => {
                send_from_0 +=1;
                tx11.send(v1).await;
                stop_flag = false;
            }
            _ = interval.tick() => {
                if stop_flag {
                    break
                }else {
                    stop_flag = true
                }
            }
        }
        println!("send from 1: {send_from_1}, send from 0: {send_from_0}")
    }
}

#[tokio::main]
async fn main() {
    let input = read_file_by_line("../inputs/day18.input");
    //let input = read_file_by_line("../inputs/day18_demo.input");
    //parse_inputs(&input);

    //day18(&input);
    day18_p2(&input).await;
}
