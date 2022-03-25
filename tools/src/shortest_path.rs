use std::collections::HashSet;
use std::hash::Hash;
use std::{
    cell::RefCell,
    collections::{BinaryHeap, HashMap},
    rc::Rc,
};

/// value pairs of each nodes in graph
#[derive(Debug)]
struct IDValuePiar<ID, V>
where
    V: Ord,
{
    inner: (ID, V),
}

impl<ID, V: Ord> IDValuePiar<ID, V> {
    #[must_use]
    fn new(id: ID, v: V) -> Self {
        Self { inner: (id, v) }
    }

    fn id(&self) -> ID {
        self.inner.0
    }

    fn v(&self) -> V {
        self.inner.1
    }
}

impl<ID, V: Ord> Eq for IDValuePiar<ID, V> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<ID, V: Ord> Ord for IDValuePiar<ID, V> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.inner.1.cmp(&self.inner.1)
    }
}

impl<ID, V: Ord> PartialOrd for IDValuePiar<ID, V> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.inner.1.partial_cmp(&self.inner.1)
    }
}

impl<ID, V: Ord> PartialEq for IDValuePiar<ID, V> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.1 == other.inner.1
    }
}

/// graph for store all nodes
struct Graph<ID, V>
where
    ID: Hash + Clone,
    V: Ord,
{
    graph: HashMap<ID, BinaryHeap<IDValuePiar<ID, V>>>,
}

impl<ID, V> Graph<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord,
{
    #[must_use]
    fn new() -> Self {
        Self {
            graph: HashMap::new(),
        }
    }

    /// insert neighbour node (one direction)
    fn insert(&mut self, id: ID, other_id: ID, v: V) {
        self.graph
            .entry(id.clone())
            .or_insert(BinaryHeap::new())
            .push(IDValuePiar::new(other_id, v));
    }

    fn get(&self, k: &ID) -> Option<&BinaryHeap<IDValuePiar<ID, V>>> {
        self.graph.get(k)
    }
}

/// Dijkstra instance
struct Dijkstra<ID, V> {
    /*/// need record from and to for each dijkstra instance
from: ID,
/// need record from and to for each dijkstra instance
to: ID,*/
/*
/// save all node results
record: HashMap<ID, V>,
/// keep the next round
next_round: BinaryHeap<V>,*/}

impl<ID, V> Dijkstra<ID, V>
where
    ID: Hash + Eq + Clone,
    V: Ord + Default,
{
    //:= todo
    fn new() {
        todo!()
    }

    fn run(&mut self, g: &Graph<ID, V>, start: ID, end: ID) {
        let mut record: HashMap<ID, V> = HashMap::new();
        let mut next_round = BinaryHeap::new();

        // record all already pass
        let mut already = HashSet::new();
        record.insert(start, V::default());
        let mut this = start;
        loop {
            if let Some(v) = record.get(&end) {
                return *v;
            }

            let this_v = record.get(&this).unwrap();
            let neighbours = g.get(&this).unwrap().clone();
            for n in neighbours {
                if !already.contains(n.id()) {
                    match record.get(n.id()) {
                        Some(old_v) => {
                            if *old_v >= this_v + n.v() {
                                record.insert(n.id(), this_v + n.v());
                                next_round.push(IDValuePiar::new(n.id(), this_v + n.v()));
                            } else {
                                next_round.push(IDValuePiar::new(n.id(), n.v()));
                            }
                        }
                        None => {
                            record.insert(n.id(), this_v + n.v());
                            next_round.push(IDValuePiar::new(n.id(), this_v + n.v()));
                        }
                    }
                }
            }

            already.insert(this);

            //:= loop next_round
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_binaryheap_with_IDValuePair() {
        let a = IDValuePiar::new("a", 1);
        let b = IDValuePiar::new("b", 2);
        let c = IDValuePiar::new("c", 3);

        let mut heap = BinaryHeap::new();
        heap.push(a);
        heap.push(b);
        heap.push(c);

        assert_eq!(heap.pop(), Some(IDValuePiar::new("a", 1)));
        assert_eq!(heap.pop(), Some(IDValuePiar::new("b", 2)));
        heap.push(IDValuePiar::new("d", 4));
        assert_eq!(heap.pop(), Some(IDValuePiar::new("c", 3)));

        // `ID` doesn't compare in Eq trait implement
        assert_eq!(heap.pop(), Some(IDValuePiar::new("1", 4)));
    }
}
