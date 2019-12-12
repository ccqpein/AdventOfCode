use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::rc::Rc;

#[derive(Debug)]
struct Cons {
    val: String,
    next: Option<Rc<RefCell<Cons>>>,
}

impl Cons {
    fn new(s: String) -> Self {
        Cons { val: s, next: None }
    }
}

#[derive(Debug)]
struct Table {
    table: HashMap<String, Rc<RefCell<Cons>>>,
}

impl Table {
    fn new() -> Self {
        let mut temp = HashMap::new();
        temp.insert(
            "COM".to_string(),
            Rc::new(RefCell::new(Cons::new("COM".to_string()))),
        );
        Table { table: temp }
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
        (*head.borrow_mut()).next = Some(next.clone());

        self.table.insert(car, head);
        self.table.insert(cadr, next);
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

fn main() {
    let mut a = Table::new();
    a.insert("aa".to_string(), "bb".to_string());

    dbg!(&a);

    (*a.table.get_mut("bb").unwrap().borrow_mut()).val = "cc".to_string();

    dbg!(&a);

    dbg!(Rc::strong_count(a.table.get_mut("bb").unwrap()));
}
