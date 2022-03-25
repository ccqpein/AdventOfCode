use std::hash::Hash;
use std::{
    cell::RefCell,
    collections::{BinaryHeap, HashMap},
    rc::Rc,
};

/// value pairs of each nodes in graph
#[derive(Ord, Eq, Debug)]
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
    ID: Hash + Eq + Ord + Clone,
    V: Ord,
{
    graph: HashMap<ID, BinaryHeap<IDValuePiar<ID, V>>>,
}

impl<ID, V> Graph<ID, V>
where
    ID: Hash + Eq + Ord + Clone,
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
}

//:= TODO
/// Dijkstra instance
struct Dijkstra {}

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
