use std::{
    cell::RefCell,
    collections::{BinaryHeap, HashMap},
    rc::Rc,
};

#[derive(Ord, Eq, Debug)]
struct IDValuePiar<ID, V: Ord> {
    inner: (ID, V),
}

impl<ID, V: Ord> PartialOrd for IDValuePiar<ID, V> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.1.partial_cmp(&other.inner.1)
    }
}

impl<ID, V: Ord> PartialEq for IDValuePiar<ID, V> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.1 == other.inner.1
    }
}

impl<ID, V: Ord> IDValuePiar<ID, V> {
    #[must_use]
    fn new(id: ID, v: V) -> Self {
        Self { inner: (id, v) }
    }
}

struct Graph<ID, V: Ord> {
    graph: HashMap<ID, BinaryHeap<IDValuePiar<ID, V>>>,
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

        assert_eq!(heap.pop(), Some(IDValuePiar::new("c", 3)));
        assert_eq!(heap.pop(), Some(IDValuePiar::new("b", 2)));
        heap.push(IDValuePiar::new("d", 4));
        assert_eq!(heap.pop(), Some(IDValuePiar::new("d", 4)));

        // `ID` doesn't compare in Eq trait implement
        assert_eq!(heap.pop(), Some(IDValuePiar::new("1", 1)));
    }
}
