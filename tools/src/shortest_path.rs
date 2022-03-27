use std::collections::HashSet;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;
use std::ops::Add;

/// value pairs of each nodes in graph
#[derive(Debug)]
pub struct IDValuePiar<ID, V>
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

    fn id(&self) -> &ID {
        &self.inner.0
    }

    fn v(&self) -> &V {
        &self.inner.1
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
pub struct Graph<ID, V>
where
    ID: Hash + Clone + Eq,
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
    pub fn new() -> Self {
        Self {
            graph: HashMap::new(),
        }
    }

    /// insert neighbour node (one direction)
    pub fn insert(&mut self, id: ID, other_id: ID, v: V) {
        self.graph
            .entry(id.clone())
            .or_insert(BinaryHeap::new())
            .push(IDValuePiar::new(other_id, v));
    }

    pub fn get(&self, k: &ID) -> Option<&BinaryHeap<IDValuePiar<ID, V>>> {
        self.graph.get(k)
    }
}

/// Dijkstra instance
pub struct Dijkstra {}

impl Dijkstra {
    pub fn new() -> Self {
        Dijkstra {}
    }

    pub fn run<ID, V>(&mut self, g: &Graph<ID, V>, start: ID, end: ID) -> V
    where
        ID: Hash + Clone + Eq + Copy,
        V: Ord + Default + Add<Output = V> + Copy,
    {
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

            let this_v = record.get(&this).unwrap().to_owned();
            let neighbours = g.get(&this).unwrap().clone();
            for n in neighbours {
                if !already.contains(n.id()) {
                    match record.get(n.id()) {
                        Some(old_v) => {
                            if *old_v >= this_v + *n.v() {
                                record.insert(n.id().clone(), this_v + *n.v());
                                next_round.push(IDValuePiar::new(n.id().clone(), this_v + *n.v()));
                            } else {
                                next_round.push(IDValuePiar::new(n.id().clone(), *old_v));
                            }
                        }
                        None => {
                            record.insert(n.id().clone(), this_v + *n.v());
                            next_round.push(IDValuePiar::new(n.id().clone(), this_v + *n.v()));
                        }
                    }
                }
            }

            already.insert(this);

            this = loop {
                match next_round.pop() {
                    Some(n) => {
                        if !already.contains(n.id()) {
                            break *n.id();
                        }
                    }
                    None => return *record.get(&end).unwrap(),
                }
            }
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
