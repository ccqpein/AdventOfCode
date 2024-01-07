use rand::seq::IteratorRandom;
use std::collections::HashSet;
use std::collections::{BinaryHeap, HashMap};
use std::error::Error;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Add;

#[derive(Debug)]
pub enum GErrorType {
    WrongType,
}

impl Display for GErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GErrorType::WrongType => write!(f, "WrongType"),
        }
    }
}

#[derive(Debug)]
pub struct GError {
    msg: String,
    ty: GErrorType,
}

impl GError {
    pub fn new(kind: GErrorType, msg: &'_ str) -> Self {
        Self {
            msg: msg.to_string(),
            ty: kind,
        }
    }
}

impl Display for GError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self, self.msg)
    }
}

impl Error for GError {}

#[derive(Clone)]
pub enum GraphType {
    Undirected,
    Directed,
}

/// value pairs of each nodes in graph
#[derive(Debug, Clone)]
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
/// V is the weight between the nodes
#[derive(Clone)]
pub struct Graph<ID, V>
where
    ID: Hash + Eq,
    V: Ord,
{
    ty: GraphType,
    graph: HashMap<ID, BinaryHeap<IDValuePiar<ID, V>>>,
}

impl<'a, ID, V> IntoIterator for &'a Graph<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone,
{
    type Item = (&'a ID, &'a BinaryHeap<IDValuePiar<ID, V>>);

    type IntoIter = std::collections::hash_map::Iter<'a, ID, BinaryHeap<IDValuePiar<ID, V>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.graph.iter()
    }
}

impl<ID, V> Graph<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone,
{
    #[must_use]
    pub fn new(ty: GraphType) -> Self {
        Self {
            ty,
            graph: HashMap::new(),
        }
    }

    /// insert neighbour node (one direction)
    pub fn insert(&mut self, id: ID, other_id: ID, v: V) {
        match self.ty {
            GraphType::Undirected => {
                self.graph
                    .entry(id.clone())
                    .or_insert(BinaryHeap::new())
                    .push(IDValuePiar::new(other_id.clone(), v.clone()));

                self.graph
                    .entry(other_id)
                    .or_insert(BinaryHeap::new())
                    .push(IDValuePiar::new(id, v));
            }
            GraphType::Directed => {
                self.graph
                    .entry(id.clone())
                    .or_insert(BinaryHeap::new())
                    .push(IDValuePiar::new(other_id, v));
            }
        }
    }

    pub fn get(&self, k: &ID) -> Option<&BinaryHeap<IDValuePiar<ID, V>>> {
        self.graph.get(k)
    }

    /// get the number of nodes of this graph
    pub fn len(&self) -> usize {
        self.graph.len()
    }

    /// return all ids of this graph
    pub fn all_ids(&self) -> impl Iterator<Item = &ID> {
        self.graph.keys()
    }

    //:= merge two nodes
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

pub struct FloydWarshall {}

impl FloydWarshall {
    pub fn new() -> Self {
        Self {}
    }

    /// this run with the graph.
    /// graph is a bit diffrent that the self node need inside the neighbours of this node
    pub fn run<ID, V>(&mut self, g: &Graph<ID, V>) -> HashMap<ID, HashMap<ID, Option<V>>>
    where
        ID: Hash + Clone + Eq + std::fmt::Debug + std::fmt::Display,
        V: Ord + Default + Add<Output = V> + Copy + std::fmt::Debug,
    {
        let mut distances: HashMap<ID, HashMap<ID, Option<V>>> = HashMap::new();

        for (this, neighbours) in g {
            for n in neighbours {
                *distances
                    .entry(this.clone())
                    .or_insert(HashMap::new())
                    .entry(n.id().clone())
                    .or_insert(Default::default()) = Some(*n.v());
            }

            // this one need to add the self node to the neighbours of self node
            distances
                .entry(this.clone())
                .or_insert(HashMap::new())
                .entry(this.clone())
                .or_insert(Default::default());
        }

        for (k, _) in g {
            for (i, _) in g {
                for (j, _) in g {
                    let x = match distances.get(i).unwrap().get(k) {
                        Some(x) => *x,
                        None => None,
                    };

                    let y = match distances.get(k).unwrap().get(j) {
                        Some(x) => *x,
                        None => None,
                    };

                    if x.is_none() || y.is_none() {
                        match distances.get_mut(i).unwrap().get_mut(j) {
                            Some(_) => (),
                            None => {
                                distances.get_mut(i).unwrap().insert(j.clone(), None);
                            }
                        }
                    } else {
                        match distances.get_mut(i).unwrap().get_mut(j) {
                            Some(a) => {
                                match a {
                                    Some(_) => *a = Some(a.unwrap().min(x.unwrap() + y.unwrap())),
                                    None => *a = Some(x.unwrap() + y.unwrap()),
                                }
                                //*a = Some(a.unwrap().min(x.unwrap() + y.unwrap()));
                            }
                            None => {
                                distances
                                    .get_mut(i)
                                    .unwrap()
                                    .insert(j.clone(), Some(x.unwrap() + y.unwrap()));
                            }
                        };
                    }
                }
            }
        }
        distances
    }
}

/// Stoer–Wagner algorithm.
/// get the solve the minimum cut problem
pub struct StoerWagner<ID, V>
where
    ID: Hash + Eq,
    V: Ord,
{
    g: Graph<ID, V>,
}

impl<ID, V> StoerWagner<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone,
{
    /// will keep one graph copy inside
    pub fn new(g: &Graph<ID, V>) -> Self {
        Self { g: g.clone() }
    }

    pub fn merge_nodes(&self, nodes: &[ID]) {}

    pub fn run(&self, g: &Graph<ID, V>) {
        let mut rng = rand::thread_rng();
        // I am not sure if it is random or not
        let start_node = g.all_ids().choose(&mut rng).unwrap();
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

    #[test]
    fn test_run_floyd_warshall() {
        let mut g = Graph::new(GraphType::Directed);

        g.insert(2, 1, 4);
        g.insert(2, 3, 3);
        g.insert(1, 3, -2);
        g.insert(3, 4, 2);
        g.insert(4, 2, -1);

        g.insert(1, 1, 0);
        g.insert(2, 2, 0);
        g.insert(3, 3, 0);
        g.insert(4, 4, 0);

        let mut fw = FloydWarshall::new();
        let table = fw.run(&g);

        dbg!(table);
    }
}
