fn day15(mut basea: u64, mut baseb: u64, iters: usize) -> u32 {
    let mut judge = 0;
    for _ in 0..iters {
        basea = (basea * 16807) % 2147483647;
        baseb = (baseb * 48271) % 2147483647;
        if (basea & 0xFFFF) == (baseb & 0xFFFF) {
            judge += 1;
        }
    }
    judge
}

fn main() {
    let basea = 699;
    let baseb = 124;
    let result = day15(basea, baseb, 40_000_000);
    println!("part1: {}", result);
}
