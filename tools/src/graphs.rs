use rand::seq::IteratorRandom;
use std::collections::HashSet;
use std::collections::{BinaryHeap, HashMap};
use std::error::Error;
use std::fmt::{Debug, Display};
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
        write!(f, "{}: {}", self.ty, self.msg)
    }
}

impl Error for GError {}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum GraphType {
    Undirected,
    Directed,
}

#[doc = r"value pairs of each nodes in graph"]
#[derive(Debug, Clone)]
pub struct IDValuePiar<ID, V>
where
    V: Ord,
{
    /// when ord is 0 (default), in BinaryHeap<IDValuePiar<ID, V>>
    /// less V will pop first. When it is 1, larger V pop first
    /// yea, I should use enum
    ord: u8,

    inner: (ID, V),
}

impl<ID, V: Ord> IDValuePiar<ID, V> {
    #[must_use]
    fn new(id: ID, v: V) -> Self {
        Self {
            ord: 0,
            inner: (id, v),
        }
    }

    fn ord(mut self, ord: u8) -> Self {
        self.ord = ord;
        self
    }

    pub fn id(&self) -> &ID {
        &self.inner.0
    }

    pub fn v(&self) -> &V {
        &self.inner.1
    }
}

impl<ID, V: Ord> Eq for IDValuePiar<ID, V> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<ID, V: Ord> Ord for IDValuePiar<ID, V> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.ord == 0 {
            other.inner.1.cmp(&self.inner.1)
        } else {
            self.inner.1.cmp(&other.inner.1)
        }
    }
}

impl<ID, V: Ord> PartialOrd for IDValuePiar<ID, V> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.ord == 0 {
            other.inner.1.partial_cmp(&self.inner.1)
        } else {
            self.inner.1.partial_cmp(&other.inner.1)
        }
    }
}

impl<ID, V: Ord> PartialEq for IDValuePiar<ID, V> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.1 == other.inner.1
    }
}

/// graph for store all nodes
/// V is the weight between the nodes
#[derive(Clone, Debug)]
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
        // default ord is 0
        self.insert_with_ord(id, other_id, v, 0);
    }

    /// Insert with the order of id-value pair
    pub fn insert_with_ord(&mut self, id: ID, other_id: ID, v: V, ord: u8) {
        match self.ty {
            GraphType::Undirected => {
                self.graph
                    .entry(id.clone())
                    .or_insert(BinaryHeap::new())
                    .push(IDValuePiar::new(other_id.clone(), v.clone()).ord(ord));

                self.graph
                    .entry(other_id)
                    .or_insert(BinaryHeap::new())
                    .push(IDValuePiar::new(id, v).ord(ord));
            }
            GraphType::Directed => {
                self.graph
                    .entry(id.clone())
                    .or_insert(BinaryHeap::new())
                    .push(IDValuePiar::new(other_id, v).ord(ord));
            }
        }
    }

    pub fn get(&self, k: &ID) -> Option<&BinaryHeap<IDValuePiar<ID, V>>> {
        self.graph.get(k)
    }

    pub fn get_mut(&mut self, k: &ID) -> Option<&mut BinaryHeap<IDValuePiar<ID, V>>> {
        self.graph.get_mut(k)
    }

    /// get the number of nodes of this graph
    pub fn len(&self) -> usize {
        self.graph.len()
    }

    /// return all ids of this graph
    pub fn all_ids(&self) -> impl Iterator<Item = &ID> {
        self.graph.keys()
    }

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
                ));
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
                    ));
                }
            }
        }

        self.graph.remove(id);
        Ok(())
    }

    /// get the ID the neighbours
    pub fn get_neighbours(&self, id: &ID) -> Option<impl IntoIterator<Item = &ID>> {
        match self.get(id) {
            Some(ns) => Some(ns.as_slice().iter().map(|n| n.id())),
            None => None,
        }
    }

    /// return id groups, those ids can visted from each other is a group
    pub fn groups(&self) -> Vec<HashSet<&ID>> {
        let mut all_ids = self.all_ids().into_iter().collect::<HashSet<_>>();
        let mut groups = vec![];
        let mut next_round = vec![];

        loop {
            // give the random start
            match all_ids.iter().next() {
                Some(x) => next_round.push(*x),
                None => return groups,
            }

            let mut group = HashSet::new();
            while !next_round.is_empty() {
                let this = next_round.pop().unwrap();
                if group.contains(this) {
                    continue;
                }

                if let Some(ns) = self.get_neighbours(this) {
                    next_round.append(&mut ns.into_iter().collect::<Vec<_>>());
                }
                group.insert(this);
                all_ids.remove(this);
            }

            if group.is_empty() {
                break;
            } else {
                groups.push(group)
            }
        }

        groups
    }
}

//struct<'a> GraphGroup<'a> {}

/// Dijkstra instance
pub struct Dijkstra {}

impl Dijkstra {
    pub fn new() -> Self {
        Dijkstra {}
    }

    pub fn run<ID, V>(&mut self, g: &Graph<ID, V>, start: ID, end: ID) -> Option<V>
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
                return Some(*v);
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
                    None => return record.get(&end).cloned(),
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
    pub fn new(g: &Graph<ID, V>) -> Result<Self, GError> {
        if g.ty != GraphType::Directed {
            return Err(GError::new(
                GErrorType::WrongType,
                "FloydWarshall has to be directed graph",
            ));
        }
        let mut g = g.clone();
        // add self node
        g.graph
            .iter_mut()
            .for_each(|(id, v)| v.push(IDValuePiar::new(id.clone(), V::default())));

        Ok(Self { g })
    }

    /// Insert for FloydWarshall.
    /// FloydWarshall's node need its own node inside
    pub fn insert(&mut self, id: ID, other_id: ID, v: V) {
        // add itself
        self.g
            .graph
            .entry(id.clone())
            .or_insert(Default::default())
            .push(IDValuePiar::new(id.clone(), V::default()));
        // add itself
        self.g
            .graph
            .entry(other_id.clone())
            .or_insert(Default::default())
            .push(IDValuePiar::new(other_id.clone(), V::default()));
        self.g.insert(id, other_id, v);
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
/// https://www.youtube.com/watch?v=AtkEpr7dsW4&t=9s
#[derive(Clone, Debug)]
pub struct StoerWagner<ID, V>
where
    ID: Hash + Clone + Eq,
    V: Ord + Clone + std::ops::Add + std::default::Default,
{
    g: Graph<ID, V>,
}

impl<ID, V> StoerWagner<ID, V>
where
    ID: Hash + Clone + Eq + Debug,
    V: Ord + Clone + Add + std::ops::AddAssign + Default + Debug,
{
    /// Will keep one graph copy inside
    pub fn new(g: &Graph<ID, V>) -> Result<Self, GError> {
        if g.ty == GraphType::Directed {
            return Err(GError::new(
                GErrorType::WrongType,
                "StoerWagner has to be undirected graph",
            ));
        }
        Ok(Self { g: g.clone() })
    }

    pub fn nodes_len(&self) -> usize {
        self.g.len()
    }

    /// Insert for StoerWagner. Since the StoerWagner need the largest weight node
    /// pop out first. It needs ord 1 when insert ID-Value pair
    pub fn insert(&mut self, id: ID, other_id: ID, v: V) {
        self.g.insert_with_ord(id, other_id, v, 1);
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
            self.insert(id1.clone(), other_id.clone(), v);
        }

        Ok(())
    }

    /// return the last-1 ID, the last ID, and the weight between them
    /// in case I need to run it one iter by one iter
    pub fn one_iter(&self, start: &ID) -> Result<(ID, ID, V), GError> {
        let mut clone_g = self.clone();
        let mut this = start.clone();
        let mut next = start.clone();
        let mut weight = Default::default();

        loop {
            match clone_g.g.get(start) {
                Some(heap) => match heap.peek() {
                    Some(next_node) => {
                        this = next;
                        next = next_node.id().clone();
                        weight = next_node.v().clone()
                    }
                    None => {
                        return Err(GError::new(
                            GErrorType::CorruptedData,
                            "heap shouldn't be empty ",
                        ));
                    }
                },
                None => return Ok((this, next, weight)),
            }

            clone_g.merge_two_nodes(start, &next)?;
        }
    }

    /// find the smallest cut
    /// if need to run one iter after iter for debugging, use one_iter
    pub fn run(&mut self, start_node: ID) -> Result<V, GError> {
        let mut result = vec![];
        loop {
            let (x, y, w) = self.one_iter(&start_node)?;
            if x == y {
                dbg!(&result);
                return Ok(result.into_iter().min().unwrap());
            }
            self.merge_two_nodes(&x, &y)?;
            result.push(w);
        }
    }

    /// Like the run, but pick the start randomly. The result should be as same as run
    pub fn run_with_random_start(&mut self) -> Result<V, GError> {
        let mut rng = rand::thread_rng();

        // I am not sure if it is random or not
        let start_node = self.g.all_ids().choose(&mut rng).unwrap().clone();
        let mut result = vec![];
        loop {
            let (x, y, w) = self.one_iter(&start_node)?;
            if x == y {
                //dbg!(&result);
                return Ok(result.into_iter().min().unwrap());
            }
            self.merge_two_nodes(&x, &y)?;
            result.push(w);
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

        // reverse
        // let mut heap = BinaryHeap::new();
        // let a = IDValuePiar::new("a", 1);
        // let b = IDValuePiar::new("b", 2);
        // let c = IDValuePiar::new("c", 3);
        // heap.push(Reverse(a));
        // heap.push(Reverse(b));
        // heap.push(Reverse(c));

        // assert_eq!(heap.pop(), Some(Reverse(IDValuePiar::new("c", 3))));
        // assert_eq!(heap.pop(), Some(Reverse(IDValuePiar::new("b", 2))));
        // assert_eq!(heap.pop(), Some(Reverse(IDValuePiar::new("whatever", 1))));
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

        let fw = FloydWarshall::new(&g).unwrap();
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
        //dbg!(sw.g.get(&'a').unwrap());
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

        sw.merge_two_nodes(&'a', &'e').unwrap();
        //dbg!(sw.g.get(&'a'));
        assert!(sw.g.get(&'a').is_none());
    }

    #[test]
    fn test_stoer_wagner_one_iter() {
        let g = Graph::new(GraphType::Undirected);

        let mut sw = StoerWagner::new(&g).unwrap();
        sw.insert('a', 'b', 5);
        sw.insert('a', 'f', 4);
        sw.insert('a', 'e', 1);
        sw.insert('f', 'c', 1);
        sw.insert('e', 'c', 1);
        sw.insert('e', 'd', 3);
        sw.insert('c', 'd', 6);
        sw.insert('c', 'b', 2);

        let (a, b, w) = sw.one_iter(&'a').unwrap();

        dbg!((a, b, w));

        sw.merge_two_nodes(&a, &b).unwrap();
        dbg!(sw.g);
    }

    #[test]
    fn test_stoer_wagner_run() {
        let g = Graph::new(GraphType::Undirected);

        let mut sw = StoerWagner::new(&g).unwrap();
        sw.insert('a', 'b', 5);
        sw.insert('a', 'f', 4);
        sw.insert('a', 'e', 1);
        sw.insert('f', 'c', 1);
        sw.insert('e', 'c', 1);
        sw.insert('e', 'd', 3);
        sw.insert('c', 'd', 6);
        sw.insert('c', 'b', 2);

        assert_eq!(sw.clone().run('a').unwrap(), 4);
        assert_eq!(sw.run_with_random_start().unwrap(), 4);
    }
}
