struct Boost {
    intcode: Vec<i64>,
    output: Vec<i64>,
    relative_base: usize,
    index: usize,
}

impl Boost {
    fn new(intcode: &Vec<i64>) -> Self {
        Boost {
            intcode: intcode.clone(),
            output: vec![],
            relative_base: 0,
            index: 0,
        }
    }

    fn run(&self) {}
}
