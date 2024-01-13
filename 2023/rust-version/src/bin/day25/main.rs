use regex::{Captures, Regex};
use tools::*;

/// for some reason, the answer isn't consistant because the graph use binary heap
/// and the same value (weigth 1) in binary heap cannot making sure which to pop first

fn day25(lines: &[String]) -> usize {
    let (mut sw, start_node) = parse_lines(lines);
    let mut step = 0;
    let nodes_len = sw.nodes_len();
    dbg!(nodes_len);
    loop {
        let (a, b, v) = match sw.one_iter(&start_node) {
            Ok(x) => x,
            Err(e) => panic!("{}", e),
        };
        step += 1;
        if v == 3 {
            dbg!((nodes_len - step));
            dbg!(step);
            return (nodes_len - step) * step;
        }
        sw.merge_two_nodes(&a, &b);
    }
}

/// test if the min_cut is 3 or not
fn min_cut(lines: &[String]) -> u64 {
    let mut sw = parse_lines(lines).0;

    sw.run_with_random_start().unwrap()
}

fn parse_lines(lines: &[String]) -> (StoerWagner<String, u64>, String) {
    let g = Graph::new(GraphType::Undirected);
    let mut sw = StoerWagner::new(&g).unwrap();
    let re = Regex::new(r"(\w+)").unwrap();
    let mut start_node = String::new();
    for line in lines {
        let mut all_match = re.find_iter(line).map(|m| m.as_str().to_string());
        let a: String = all_match.next().unwrap();
        if start_node.is_empty() {
            start_node = a.clone();
        }
        for b in all_match {
            sw.insert(a.clone(), b, 1_u64);
        }
    }

    (sw, start_node)
}

fn main() {
    let input = read_file_by_line("../inputs/day25_demo.input");
    let input = read_file_by_line("../inputs/day25.input");
    //parse_lines(&input);
    //dbg!(min_cut(&input));
    dbg!(day25(&input));
}
