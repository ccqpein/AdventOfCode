use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::rc::Rc;

#[derive(Debug)]
struct Cons {
    val: String,
    next: Vec<Rc<RefCell<Cons>>>,
}

impl Cons {
    fn new(s: String) -> Self {
        Cons {
            val: s,
            next: vec![],
        }
    }

    fn get_val(&self) -> String {
        self.val.to_string()
    }
}

#[derive(Debug)]
struct Table {
    table: HashMap<String, Rc<RefCell<Cons>>>,

    total_orbits: HashMap<String, i32>,

    path_reverse: HashMap<String, String>,
}

impl Table {
    fn new() -> Self {
        let mut temp = HashMap::new();
        temp.insert(
            "COM".to_string(),
            Rc::new(RefCell::new(Cons::new("COM".to_string()))),
        );
        Table {
            table: temp,
            total_orbits: HashMap::new(),
            path_reverse: HashMap::new(),
        }
    }

    fn insert(&mut self, car: String, cadr: String) {
        // go find next first
        let next = match self.table.get(&cadr) {
            None => Rc::new(RefCell::new(Cons::new(cadr.clone()))),
            Some(cons) => cons.clone(),
        };

        let head = match self.table.get(&car) {
            None => Rc::new(RefCell::new(Cons::new(car.clone()))),
            Some(head) => head.clone(),
        };

        // link head to next
        (*head.borrow_mut()).next.push(next.clone());

        self.table.insert(car.clone(), head);
        self.table.insert(cadr.clone(), next);

        // record reverse path
        self.path_reverse.insert(cadr, car);
    }

    // recursive from com to the end
    fn find_orbits(&mut self, key: &str, sum: i32) {
        self.total_orbits.insert(key.to_string(), sum);

        let next_set = self
            .table
            .get(key)
            .unwrap()
            .borrow()
            .next
            .iter()
            .map(|c| c.borrow().get_val())
            .collect::<Vec<String>>();

        for s in next_set {
            self.find_orbits(&s, sum + 1)
        }
    }

    fn find_path(&self, key: String) -> Vec<String> {
        let mut result = vec![];
        let mut key = key;
        loop {
            match self.path_reverse.get(&key) {
                Some(p) if p != "COM" => {
                    result.push(p.to_string());
                    key = p.to_string();
                }
                _ => {
                    break;
                }
            }
        }

        result.reverse();
        result
    }
}

fn parse_line(s: String) -> Vec<String> {
    s.split(')').map(|x| x.to_string()).collect::<Vec<String>>()
}

fn read_file_by_line(filepath: String) -> Vec<String> {
    let file = File::open(filepath).unwrap();
    BufReader::new(file)
        .lines()
        .into_iter()
        .map(|l| l.unwrap())
        .collect::<Vec<String>>()
}

fn day6(filepath: String) {
    let pairs = read_file_by_line(filepath)
        .iter()
        .map(|s| parse_line(s.to_string()))
        .collect::<Vec<Vec<String>>>();

    let mut tb = Table::new();

    for v in pairs {
        tb.insert(v[0].clone(), v[1].clone());
    }

    tb.find_orbits("COM", 0);

    //dbg!(tb);
    let temp = tb.total_orbits.iter().map(|a| *a.1).collect::<Vec<i32>>();
    dbg!(temp.as_slice().iter().sum::<i32>());
}

fn day6_part2(filepath: String) {
    let pairs = read_file_by_line(filepath)
        .iter()
        .map(|s| parse_line(s.to_string()))
        .collect::<Vec<Vec<String>>>();

    let mut tb = Table::new();

    for v in pairs {
        tb.insert(v[0].clone(), v[1].clone());
    }

    tb.find_orbits("COM", 0);

    let path0 = tb.find_path("YOU".to_string());
    let path1 = tb.find_path("SAN".to_string());

    let ind = {
        let mut ind = 0;
        while path0[ind] == path1[ind] {
            ind += 1
        }
        ind
    };

    dbg!((path0.len() - ind) + (path1.len() - ind));
}

fn main() {
    // let mut a = Table::new();
    // a.insert("aa".to_string(), "bb".to_string());

    // dbg!(&a);

    // (*a.table.get_mut("bb").unwrap().borrow_mut()).val = "cc".to_string();

    // dbg!(&a);

    // dbg!(Rc::strong_count(a.table.get_mut("bb").unwrap()));

    day6_part2("day6.input".to_string());
}
