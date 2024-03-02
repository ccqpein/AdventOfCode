use md5::*;
use tools::*;

fn day4(input: &str) -> usize {
    for n in 1.. {
        let r = format!("{:x}", md5::compute(input.to_string() + &n.to_string()));
        if r[0..5] == *"00000" {
            return n;
        }
    }
    0
}

fn day4_part2(input: &str) -> usize {
    for n in 1.. {
        let r = format!("{:x}", md5::compute(input.to_string() + &n.to_string()));
        if r[0..6] == *"000000" {
            return n;
        }
    }
    0
}

fn main() {
    // println!(
    //     "{:?}",
    //     format!("{:x}", md5::compute("abcdef609043"))[0..5] == *"00000"
    // );
    println!("{:?}", day4("ckczppom"));
    println!("{:?}", day4_part2("ckczppom"));
}
