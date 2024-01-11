use rand::seq::IteratorRandom;
use std::collections::HashSet;
use std::collections::{BinaryHeap, HashMap};
use std::error::Error;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Add;

#[derive(Debug, PartialEq, Eq)]
pub enum GErrorType {
    WrongType,
    MergeFailed,
    DeleteFailed,

    CorruptedData,
}

impl Display for GErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GErrorType::WrongType => write!(f, "WrongType"),
            GErrorType::MergeFailed => write!(f, "MergeFailed"),
            GErrorType::DeleteFailed => write!(f, "DeleteFailed"),
            GErrorType::CorruptedData => write!(f, "CorruptedData"),
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

#[derive(Clone, PartialEq, Eq)]
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

impl<'a, ID, V> IntoIterator for &'a mut Graph<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone,
{
    type Item = (&'a ID, &'a mut BinaryHeap<IDValuePiar<ID, V>>);

    type IntoIter = std::collections::hash_map::IterMut<'a, ID, BinaryHeap<IDValuePiar<ID, V>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.graph.iter_mut()
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

    //:= need test
    /// delete the node from graph. only for undirected graph so far.
    /// because find which node direct to this node is expensive by now (2024-01-07).
    pub fn delete_node(&mut self, id: &ID) -> Result<(), GError> {
        if self.ty != GraphType::Undirected {
            return Err(GError::new(
                GErrorType::WrongType,
                "delete only for undirection graph",
            ));
        }

        let all_connects = match self.graph.get(id) {
            Some(h) => h.iter().map(|bh| bh.id().clone()).collect::<Vec<_>>(),
            None => {
                return Err(GError::new(
                    GErrorType::CorruptedData,
                    "Cannot find the delete node",
                ))
            }
        };

        // remove all leaves nodes' records of this node
        for n in all_connects {
            match self.graph.get_mut(&n) {
                Some(heap) => heap.retain(|e| e.id() != id),
                None => {
                    return Err(GError::new(
                        GErrorType::CorruptedData,
                        "Cannot find the node of delete node",
                    ))
                }
            }
        }

        self.graph.remove(id);
        Ok(())
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

#[derive(Clone)]
pub struct FloydWarshall<ID, V>
where
    ID: Hash + Eq,
    V: Ord,
{
    g: Graph<ID, V>,
}

impl<ID, V> FloydWarshall<ID, V>
where
    ID: Hash + Clone + Eq + std::fmt::Debug + std::fmt::Display,
    V: Ord + Default + Add<Output = V> + Copy + std::fmt::Debug,
{
    pub fn new(g: &Graph<ID, V>) -> Self {
        let mut g = g.clone();
        // add self node
        g.graph
            .iter_mut()
            .for_each(|(id, v)| v.push(IDValuePiar::new(id.clone(), V::default())));

        Self { g: g.clone() }
    }

    /// this run with the graph.
    pub fn run(&self) -> HashMap<ID, HashMap<ID, Option<V>>> {
        let mut distances: HashMap<ID, HashMap<ID, Option<V>>> = HashMap::new();

        for (this, neighbours) in &self.g {
            for n in neighbours.iter() {
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

        for (k, _) in &self.g {
            for (i, _) in &self.g {
                for (j, _) in &self.g {
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
    ID: Hash + Clone + Eq,
    V: Ord + Clone + std::ops::Add + std::ops::AddAssign + std::default::Default,
{
    g: Graph<ID, V>,
}

impl<ID, V> StoerWagner<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone + std::ops::Add + std::ops::AddAssign + std::default::Default,
{
    /// will keep one graph copy inside
    pub fn new(g: &Graph<ID, V>) -> Result<Self, GError> {
        if g.ty == GraphType::Directed {
            return Err(GError::new(
                GErrorType::WrongType,
                "StoerWagner has to be undirected graph",
            ));
        }
        Ok(Self { g: g.clone() })
    }

    /// merge two nodes, the new id is the first id
    /// it doesn't check if the id1 and id2 connect directly or not.
    pub fn merge_two_nodes(&mut self, id1: &ID, id2: &ID) -> Result<(), GError> {
        // get all connections
        let mut table = self
            .g
            .get(id1)
            .map(|heap| {
                heap.iter()
                    .map(|p| (p.id().clone(), p.v().clone()))
                    .collect::<HashMap<_, _>>()
            })
            .unwrap_or(HashMap::new());

        if let Some(v) = self.g.get(id2) {
            for vv in v {
                *table.entry(vv.id().clone()).or_insert(Default::default()) += vv.v().clone()
            }
        }

        // no need their own
        table.remove(id1);
        table.remove(id2);

        // delete the id2 and id1
        self.g.delete_node(id2)?;
        self.g.delete_node(id1)?;

        // re-insert the id1
        for (other_id, v) in table {
            self.g.insert(id1.clone(), other_id.clone(), v);
        }

        Ok(())
    }

    /// return the last-1 ID, the last ID, and the weight between them
    /// in case I need to run it one iter by one iter
    pub fn one_iter(&self, start: &ID) -> (ID, ID, V) {
        let mut clone_g = self.clone();
        todo!()
    }

    /// find the smallest cut
    /// if need to run one iter after iter for debugging, use one_iter
    pub fn run(&self, g: &Graph<ID, V>) {
        let mut rng = rand::thread_rng();
        // I am not sure if it is random or not
        let start_node = g.all_ids().choose(&mut rng).unwrap();

        //:= next start here, the merge nodes is done
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

        // from 2024-01-07 don't need to add self node
        // g.insert(1, 1, 0);
        // g.insert(2, 2, 0);
        // g.insert(3, 3, 0);
        // g.insert(4, 4, 0);

        let fw = FloydWarshall::new(&g);
        let table = fw.run();

        dbg!(table);
    }

    #[test]
    fn test_stoer_wagner_merge_two_nodes() {
        let mut g = Graph::new(GraphType::Undirected);

        g.insert('a', 'b', 5);
        g.insert('a', 'f', 4);
        g.insert('a', 'e', 1);
        g.insert('f', 'c', 1);
        g.insert('e', 'c', 1);
        g.insert('e', 'd', 3);
        g.insert('c', 'd', 6);
        g.insert('c', 'b', 2);

        let mut sw = StoerWagner::new(&g).unwrap();

        sw.merge_two_nodes(&'a', &'b').unwrap();
        //dbg!(sw.g.get(&'a'));
        assert_eq!(
            sw.g.get(&'a')
                .unwrap()
                .iter()
                .map(|pair| (pair.id().clone(), pair.v().clone()))
                .collect::<HashMap<_, _>>(),
            vec![('e', 1), ('f', 4), ('c', 2)].into_iter().collect()
        );

        sw.merge_two_nodes(&'a', &'f').unwrap();
        //dbg!(sw.g.get(&'a'));
        assert_eq!(
            sw.g.get(&'a')
                .unwrap()
                .iter()
                .map(|pair| (pair.id().clone(), pair.v().clone()))
                .collect::<HashMap<_, _>>(),
            vec![('e', 1), ('c', 3)].into_iter().collect()
        );

        sw.merge_two_nodes(&'a', &'c').unwrap();
        //dbg!(sw.g.get(&'a'));
        assert_eq!(
            sw.g.get(&'a')
                .unwrap()
                .iter()
                .map(|pair| (pair.id().clone(), pair.v().clone()))
                .collect::<HashMap<_, _>>(),
            vec![('e', 2), ('d', 6)].into_iter().collect()
        );

        sw.merge_two_nodes(&'a', &'d').unwrap();
        //dbg!(sw.g.get(&'a'));
        assert_eq!(
            sw.g.get(&'a')
                .unwrap()
                .iter()
                .map(|pair| (pair.id().clone(), pair.v().clone()))
                .collect::<HashMap<_, _>>(),
            vec![('e', 5),].into_iter().collect()
        );
    }
}
