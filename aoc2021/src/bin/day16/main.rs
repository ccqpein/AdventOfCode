use hex;
use tools::*;

// fn part1(input: &Vec<String>) -> usize {
//     let mut input = decode(&input[0]);
//     let length = input.len();
//     read_packets_by_count(&mut input, 1).0
// }

// fn read_packets_by_length(mut input: String, mut length: usize) -> usize {
//     println!("read package {}, need length {}", input, length);
//     let mut result = 0;
//     while length != 0 {
//         let version = input.drain(0..3).as_str().to_owned();
//         println!("version: {}", version);
//         result += to_decimal(&version);
//         let type_id = input.drain(0..3).as_str().to_owned();
//         length -= 6;
//         match type_id.as_str() {
//             "100" => {
//                 let mut literal_value = String::new();
//                 loop {
//                     let group = input.drain(0..5).as_str().to_owned();
//                     if group[0..1] == *"1" {
//                         literal_value += group.get(1..).unwrap();
//                         length -= 5;
//                     } else {
//                         literal_value += group.get(1..).unwrap();
//                         length -= 5;
//                         break;
//                     }
//                 }
//                 //result += to_decimal(&literal_value);
//             }
//             _ => {
//                 let length_type_ID = input.drain(0..1).as_str().to_owned();
//                 length -= 1;

//                 if &length_type_ID == "0" {
//                     let next_packages = to_decimal(input.drain(0..15).as_str());
//                     println!("next_packages: {}", next_packages);
//                     length -= 15;

//                     result += read_packets_by_length(
//                         input.drain(0..next_packages as usize).as_str().to_owned(),
//                         next_packages as usize,
//                     );

//                     length -= next_packages as usize;
//                 } else if &length_type_ID == "1" {
//                     //let a = input.drain(0..11).as_str().to_owned();
//                     let next_packages_num = to_decimal(input.drain(0..11).as_str());
//                     length -= 11;
//                     println!("next_packages_num: {}", next_packages_num);

//                     let (re, lcost) = read_packets_by_count(&mut input, next_packages_num as usize);
//                     result += re;
//                     println!("cost: {}, input: {}", lcost, input);
//                     length -= lcost;
//                 }
//             }
//         }
//     }
//     result
// }

// fn read_packets_by_count(mut input: &mut String, mut count: usize) -> (usize, usize) {
//     println!("read package {}, need count {}", input, count);
//     let mut result = 0;
//     let mut length: usize = 0;
//     while count != 0 {
//         let version = input.drain(0..3).as_str().to_owned();
//         println!("version: {}", version);
//         result += to_decimal(&version);
//         let type_id = input.drain(0..3).as_str().to_owned();
//         length += 6;
//         match type_id.as_str() {
//             "100" => {
//                 let mut literal_value = String::new();
//                 loop {
//                     let group = input.drain(0..5).as_str().to_owned();
//                     if group[0..1] == *"1" {
//                         literal_value += group.get(1..).unwrap();
//                         length += 5;
//                     } else {
//                         literal_value += group.get(1..).unwrap();
//                         length += 5;
//                         break;
//                     }
//                 }
//                 count -= 1;
//                 //result += to_decimal(&literal_value);
//             }
//             _ => {
//                 let length_type_ID = input.drain(0..1).as_str().to_owned();
//                 length += 1;

//                 if &length_type_ID == "0" {
//                     let next_packages = to_decimal(input.drain(0..15).as_str());
//                     println!("next_packages: {}", next_packages);
//                     length += 15;

//                     result += read_packets_by_length(
//                         input.drain(0..next_packages as usize).as_str().to_owned(),
//                         next_packages as usize,
//                     );
//                     count -= 1;
//                     length += next_packages as usize
//                 } else if &length_type_ID == "1" {
//                     let next_packages_num = to_decimal(input.drain(0..11).as_str());
//                     length += 11;
//                     println!("next_packages_num: {}", next_packages_num);
//                     let (re, lcost) = read_packets_by_count(input, next_packages_num as usize);
//                     result += re;
//                     length += lcost;
//                     count -= 1;
//                 }
//             }
//         }
//     }
//     (result, length)
// }

// fn read_packets_by_count_part2(mut input: &mut String, count: usize) -> Vec<i32> {
//     //let mut result = 0;
//     let version = input.drain(0..3).as_str().to_owned();
//     println!("version: {}", version);
//     let type_id = input.drain(0..3).as_str().to_owned();
//     let mut cache = vec![];
//     while count != 0 {
//         match type_id.as_str() {
//             "100" => {
//                 let mut literal_value = String::new();
//                 loop {
//                     let group = input.drain(0..5).as_str().to_owned();
//                     if group[0..1] == *"1" {
//                         literal_value += group.get(1..).unwrap();
//                     } else {
//                         literal_value += group.get(1..).unwrap();
//                         break;
//                     }
//                 }
//                 cache.push(to_decimal(&literal_value));
//             }
//             id @ _ => {
//                 let length_type_ID = input.drain(0..1).as_str().to_owned();
//                 let results = if &length_type_ID == "0" {
//                     let next_packages = to_decimal(input.drain(0..15).as_str());
//                     println!("next_packages: {}", next_packages);

//                     read_packets_by_length_p2(
//                         input.drain(0..next_packages as usize).as_str().to_owned(),
//                         next_packages as usize,
//                     )
//                 } else {
//                     let next_packages_num = to_decimal(input.drain(0..11).as_str());

//                     println!("next_packages_num: {}", next_packages_num);
//                     read_packets_by_count_part2(input, next_packages_num as usize)
//                 };

//                 let result = match id {
//                     "000" => results.iter().sum(),
//                     "001" => results.iter().product(),
//                     "010" => *results.iter().min().unwrap(),
//                     "011" => *results.iter().max().unwrap(),
//                     "101" => {
//                         if results[0] > results[1] {
//                             1
//                         } else {
//                             0
//                         }
//                     }
//                     "110" => {
//                         if results[0] < results[1] {
//                             1
//                         } else {
//                             0
//                         }
//                     }
//                     "111" => {
//                         if results[0] == results[1] {
//                             1
//                         } else {
//                             0
//                         }
//                     }
//                 };
//                 cache.push(result)
//             }
//         }
//     }
//     cache
// }

// fn read_packets_by_length_p2(mut input: String, mut length: usize) -> Vec<i32> {
//     println!("read package {}, need length {}", input, length);
//     let mut cache = vec![];
//     while length != 0 {
//         let version = input.drain(0..3).as_str().to_owned();
//         println!("version: {}", version);
//         let type_id = input.drain(0..3).as_str().to_owned();
//         length -= 6;
//         match type_id.as_str() {
//             "100" => {
//                 let mut literal_value = String::new();
//                 loop {
//                     let group = input.drain(0..5).as_str().to_owned();
//                     if group[0..1] == *"1" {
//                         literal_value += group.get(1..).unwrap();
//                         length -= 5;
//                     } else {
//                         literal_value += group.get(1..).unwrap();
//                         length -= 5;
//                         break;
//                     }
//                 }
//                 cache.push(to_decimal(&literal_value));
//             }
//             id @ _ => {
//                 let length_type_ID = input.drain(0..1).as_str().to_owned();
//                 length -= 1;

//                 if &length_type_ID == "0" {
//                     let next_packages = to_decimal(input.drain(0..15).as_str());
//                     println!("next_packages: {}", next_packages);
//                     length -= 15;
//                     length -= next_packages as usize;
//                     cache.append(&mut read_packets_by_length_p2(
//                         input.drain(0..next_packages as usize).as_str().to_owned(),
//                         next_packages as usize,
//                     ));
//                 } else {
//                     //let a = input.drain(0..11).as_str().to_owned();
//                     let next_packages_num = to_decimal(input.drain(0..11).as_str());
//                     length -= 11;
//                     println!("next_packages_num: {}", next_packages_num);

//                     read_packets_by_count_p2(&mut input, next_packages_num as usize);
//                 }
//             }
//         }
//     }
//     cache
// }

fn part1(input: &Vec<String>) {
    let mut input = decode(&input[0]);
    let root_packet = Packet::from_input(&mut input);
    println!(
        "input {} result: {}",
        input,
        part1_packet_handler(&root_packet)
    )
}

fn part1_packet_handler(p: &Packet) -> usize {
    to_decimal(&p.version)
        + match &p.sub_packets {
            Some(pl) => pl.iter().map(|pp| part1_packet_handler(pp)).sum::<usize>(),
            None => 0,
        }
}

fn part2(input: &Vec<String>) {
    for a in input {
        println!("input: {}", a);
        let root_packet = Packet::from_input(&mut decode(a));
        println!("input {} result: {}", a, part2_packet_handler(&root_packet))
    }
}

fn part2_packet_handler(p: &Packet) -> usize {
    let result: usize = match p.type_id.as_str() {
        "000" => p
            .sub_packets
            .as_ref()
            .unwrap()
            .iter()
            .map(|p| part2_packet_handler(p))
            .sum(),
        "001" => p
            .sub_packets
            .as_ref()
            .unwrap()
            .iter()
            .map(|p| part2_packet_handler(p))
            .product(),
        "010" => p
            .sub_packets
            .as_ref()
            .unwrap()
            .iter()
            .map(|p| part2_packet_handler(p))
            .min()
            .unwrap(),
        "011" => p
            .sub_packets
            .as_ref()
            .unwrap()
            .iter()
            .map(|p| part2_packet_handler(p))
            .max()
            .unwrap(),
        "100" => *p.literal_value.as_ref().unwrap(),
        "101" => {
            if part2_packet_handler(&p.sub_packets.as_ref().unwrap()[0])
                > part2_packet_handler(&p.sub_packets.as_ref().unwrap()[1])
            {
                1
            } else {
                0
            }
        }
        "110" => {
            if part2_packet_handler(&p.sub_packets.as_ref().unwrap()[0])
                < part2_packet_handler(&p.sub_packets.as_ref().unwrap()[1])
            {
                1
            } else {
                0
            }
        }
        "111" => {
            if part2_packet_handler(&p.sub_packets.as_ref().unwrap()[0])
                == part2_packet_handler(&p.sub_packets.as_ref().unwrap()[1])
            {
                1
            } else {
                0
            }
        }
        _ => {
            panic!()
        }
    };
    //println!("input {:?} result: {}", p, result);
    result
}

#[derive(Debug)]
struct Packet {
    version: String,
    type_id: String,

    literal_value: Option<usize>,

    sub_packets: Option<Vec<Packet>>,
}

impl Packet {
    fn new(version: String, type_id: String) -> Self {
        Self {
            version,
            type_id,
            literal_value: None,
            sub_packets: None,
        }
    }

    fn from_input(input: &mut String) -> Self {
        //println!("input now: {}", input);
        let version = input.drain(0..3).as_str().to_owned();
        let type_id = input.drain(0..3).as_str().to_owned();

        let mut literal_value = None;
        let mut sub_packets = vec![];
        match type_id.as_str() {
            "100" => {
                let mut literal = String::new();
                loop {
                    let group = input.drain(0..5).as_str().to_owned();
                    if group[0..1] == *"1" {
                        literal += group.get(1..).unwrap();
                    } else {
                        literal += group.get(1..).unwrap();
                        break;
                    }
                }
                literal_value = Some(to_decimal(&literal))
            }
            _ => {
                let length_type_id = input.drain(0..1).as_str().to_owned();
                if &length_type_id == "0" {
                    let next_packages_length = to_decimal(input.drain(0..15).as_str());
                    //println!("next_packages_length: {}", next_packages_length);

                    let mut length_now = input.len().to_owned();
                    let target = (length_now - next_packages_length as usize);
                    while length_now != target {
                        sub_packets.push(Self::from_input(input));
                        length_now = input.len().to_owned();
                    }
                } else {
                    let next_packages_num = to_decimal(input.drain(0..11).as_str());
                    //println!("next_packages_num: {}", next_packages_num);
                    for _ in 0..next_packages_num {
                        sub_packets.push(Self::from_input(input))
                    }
                };
            }
        };

        Self {
            version,
            type_id,
            literal_value,
            sub_packets: if !sub_packets.is_empty() {
                Some(sub_packets)
            } else {
                None
            },
        }
    }
}

fn decode(s: &str) -> String {
    hex::decode(s)
        .unwrap()
        .iter()
        .fold(String::new(), |mut acc, b| {
            //println!("{:08b}", b);
            acc += format!("{:08b}", b).as_str();
            acc
        })
}

fn to_decimal(s: &str) -> usize {
    usize::from_str_radix(s, 2).unwrap()
}

fn main() {
    let input = read_file_by_line("./src/bin/day16/day16.input");
    //let input = read_file_by_line("./src/bin/day16/day16_demo.input");

    //println!("{:b}", i64::from_str_radix("D2FE28", 16).unwrap());
    //println!("decode: {}", decode("D2FE28"));
    //println!("to_decimal: {}", to_decimal("00000000011"));
    println!("{}", decode(&input[0]));

    println!("{:?}", part1(&input));
    println!("{:?}", part2(&input));
}
