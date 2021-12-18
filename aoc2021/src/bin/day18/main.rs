//#![feature(box_syntax)]
#![feature(cell_leak)]
use std::{
    cell::{RefCell, RefMut},
    fmt,
    rc::Rc,
};

use tools::*;

#[derive(Debug)]
enum Node {
    V(i32),
    P(SFTree),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::V(v) => write!(f, "{}", v),
            Node::P(t) => write!(f, "[{}, {}]", t.left.borrow(), t.right.borrow()),
        }
    }
}

impl Node {
    fn get_n_level(&self, level: usize) -> Option<Vec<Rc<RefCell<Self>>>> {
        if let Node::P(t) = self {
            t.get_n_level(level)
        } else {
            None
        }
    }

    fn is_tree(&self) -> bool {
        if let Node::P(_) = self {
            true
        } else {
            false
        }
    }

    fn is_value(&self) -> bool {
        if let Node::V(_) = self {
            true
        } else {
            false
        }
    }

    fn get_tree(&self) -> Option<&SFTree> {
        match self {
            Node::V(_) => None,
            Node::P(tree) => Some(&tree),
        }
    }

    fn get_tree_mut(&mut self) -> Option<&mut SFTree> {
        match self {
            Node::V(_) => None,
            Node::P(ref mut tree) => Some(tree),
        }
    }

    fn update_parent(
        this: Rc<RefCell<Self>>,
        parent: Option<Rc<RefCell<Self>>>,
        on: u8,
    ) -> Rc<RefCell<Self>> {
        if this.borrow().is_tree() {
            RefCell::borrow_mut(&this)
                .get_tree_mut()
                .unwrap()
                .update_parent(parent, on);
        }

        if this.borrow().is_tree() {
            Self::update_parent(
                this.borrow().get_tree().unwrap().left.clone(),
                Some(this.clone()),
                1,
            );
            Self::update_parent(
                this.borrow().get_tree().unwrap().right.clone(),
                Some(this.clone()),
                2,
            );
        }
        this
    }

    fn parent(&self) -> Option<Rc<RefCell<Node>>> {
        self.get_tree().map(|tree| tree.parent()).unwrap()
    }
}

#[derive(Debug)]
struct SFTree {
    parent: Option<Rc<RefCell<Node>>>,
    posioin: u8, // 1 means on parent left, 2 means on parent right
    left: Rc<RefCell<Node>>,
    right: Rc<RefCell<Node>>,
}

impl fmt::Display for SFTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}, {}]", self.left.borrow(), self.right.borrow())
    }
}

impl SFTree {
    fn new(left: Node, right: Node) -> Self {
        Self {
            parent: None,
            posioin: 0,
            left: Rc::new(RefCell::new(left)),
            right: Rc::new(RefCell::new(right)),
        }
    }

    fn update_parent(&mut self, parent: Option<Rc<RefCell<Node>>>, on: u8) {
        self.parent = parent;
        self.posioin = on;
    }

    fn parent(&self) -> Option<Rc<RefCell<Node>>> {
        self.parent.clone()
    }

    fn addition(a: Self, b: Self) -> Self {
        Self::new(Node::P(a), Node::P(b))
    }

    fn get_n_level(&self, level: usize) -> Option<Vec<Rc<RefCell<Node>>>> {
        println!("tree: {}, level:{}", self, level);
        if level == 1 {
            return Some(vec![Rc::clone(&self.left), Rc::clone(&self.right)]);
        } else {
            let mut a = self.left.borrow().get_n_level(level - 1);
            let mut b = self.right.borrow().get_n_level(level - 1);
            match (a.as_mut(), b.as_mut()) {
                (None, None) => None,
                (None, Some(_)) => b,
                (Some(_), None) => a,
                (Some(aa), Some(bb)) => {
                    aa.append(bb);
                    Some(aa.to_vec())
                }
            }
        }
    }

    fn find_left(&self) -> Option<Rc<RefCell<Self>>> {
        // root node
        if self.posioin == 0 {}
        if self.posioin == 2 {}
    }
}

fn parse_input(s: &str) -> SFTree {
    if let Node::P(tree) = make_sftree(&mut s.chars().filter(|c| *c != ',')) {
        tree
    } else {
        panic!()
    }
}

fn make_sftree(input: &mut impl Iterator<Item = char>) -> Node {
    loop {
        match input.next() {
            Some('[') => return Node::P(SFTree::new(make_sftree(input), make_sftree(input))),
            Some(',') => {
                continue;
            }
            Some(']') => {
                continue;
            }
            Some(d) => {
                if let Ok(v) = d.to_string().parse::<i32>() {
                    return Node::V(v);
                } else {
                    panic!()
                }
            }
            _ => unreachable!(),
        }
    }
}

fn part1(input: &Vec<String>) {}

fn main() {
    let input = read_file_by_line("./src/bin/day18/day18.input");
    let input = read_file_by_line("./src/bin/day18/day18_demo.input");

    let t = parse_input("[[[[4,3],4],4],[7,[[8,4],9]]]");
    //dbg!(t.get_n_level(4));
    //println!("{}", t);

    // let t = Rc::new(RefCell::new(Node::P(parse_input(
    //     "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]",
    // ))));
    //Node::update_parent(Rc::clone(&t.left), None);
    //Node::update_parent(Rc::clone(&t.right), None);
    // let t = Node::update_parent(t, None);
    // println!("{}", t.as_ref().borrow());
    // let l4 = t.borrow().get_n_level(4).unwrap();
    // for t in &l4 {
    //     println!("{}", t.borrow());
    // }
    // println!(
    //     "{}",
    //     l4[0]
    //         .borrow()
    //         .parent()
    //         .unwrap()
    //         .borrow()
    //         .get_tree()
    //         .unwrap()
    // );

    let t = Rc::new(RefCell::new(Node::P(parse_input(
        "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
    ))));
    let t = Node::update_parent(t, None);
    let l4 = t.borrow().get_n_level(4).unwrap();
    for t in &l4 {
        println!("{}", t.borrow());
    }

    //println!("{:?}", part1(&input));
    //println!("{:?}", part2(&input));
}
