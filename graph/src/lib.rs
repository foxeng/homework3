use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs;
use std::hash::Hash;

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

#[derive(Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
struct VertexIden(usize);
#[derive(Hash, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
struct EdgeIden(usize);

#[derive(Default, Serialize, Deserialize)]
#[serde(bound = "V: Serialize + DeserializeOwned, E: Serialize + DeserializeOwned")]
pub struct Graph<V, E>
where
    V: Hash + Eq + Clone + Serialize + DeserializeOwned,
    E: Hash + Eq + Clone + Serialize + DeserializeOwned,
{
    vtoi: HashMap<V, VertexIden>,
    itov: HashMap<VertexIden, V>,
    etoi: HashMap<E, EdgeIden>,
    itoe: HashMap<EdgeIden, E>,
    adj: HashMap<VertexIden, HashMap<VertexIden, EdgeIden>>,
}

impl<V, E> Graph<V, E>
where
    V: Hash + Eq + Clone + Serialize + DeserializeOwned,
    E: Hash + Eq + Clone + Serialize + DeserializeOwned,
{
    pub fn new() -> Graph<V, E> {
        Graph {
            vtoi: HashMap::new(),
            itov: HashMap::new(),
            etoi: HashMap::new(),
            itoe: HashMap::new(),
            adj: HashMap::new(),
        }
    }

    pub fn add_vertex(&mut self, v: V) {
        let vi = VertexIden(self.vtoi.len());
        let v2 = v.clone();
        self.vtoi.insert(v, vi);
        self.itov.insert(vi, v2);
        // TODO OPT: Insert empty map in adj to establish invariant?
    }

    /// Panics if v1, v2 not in the graph. Does nothing if edge already exists
    /// between v1 and v2.
    pub fn add_edge(&mut self, v1: &V, v2: &V, e: E) {
        let vi1 = self.vtoi[v1];
        let vi2 = self.vtoi[v2];
        if let Some(h) = self.adj.get(&vi1) {
            if h.contains_key(&vi2) {
                // Duplicate edge
                return;
            }
        }

        let ei = EdgeIden(self.etoi.len());
        let e2 = e.clone();
        self.etoi.insert(e, ei);
        self.itoe.insert(ei, e2);
        self.adj
            .entry(vi1)
            .and_modify(|h| {
                h.insert(vi2, ei);
            })
            .or_insert_with(|| HashMap::from([(vi2, ei)]));
    }

    pub fn ensure_vertex(&mut self, v: V) -> bool {
        // TODO OPT: Accept function instead of V

        if self.has_vertex(&v) {
            return true;
        }

        self.add_vertex(v);
        false
    }

    pub fn ensure_edge(&mut self, v1: &V, v2: &V, e: E) -> bool {
        // TODO OPT: Accept function instead of E

        if self.has_edge(v1, v2) {
            return true;
        }

        self.add_edge(v1, v2, e);
        false
    }

    pub fn remove_vertex(&mut self, v: &V) {
        let vi = match self.vtoi.get(v) {
            None => return,
            Some(&vi) => vi,
        };

        // Remove all out-edges
        if let Some(h) = self.adj.remove(&vi) {
            for ei in h.into_values() {
                let e = &self.itoe[&ei];
                self.etoi.remove(e);
                self.itoe.remove(&ei);
            }
        }
        // Remove all in-edges
        for h in self.adj.values_mut() {
            if let Some(ei) = h.remove(&vi) {
                let e = &self.itoe[&ei];
                self.etoi.remove(e);
                self.itoe.remove(&ei);
            }
        }

        // Remove vertex
        self.vtoi.remove(v);
        self.itov.remove(&vi);
    }

    pub fn remove_edge(&mut self, e: &E) {
        let ei = match self.etoi.get(e) {
            None => return,
            Some(&ei) => ei,
        };

        for h in self.adj.values_mut() {
            h.retain(|_, v| *v != ei);
        }

        self.etoi.remove(e);
        self.itoe.remove(&ei);
    }

    /// Panics if v not in the graph.
    pub fn degree_out(&self, v: &V) -> usize {
        // TODO OPT: Return option instead of panicing?
        let vi = self.vtoi[v];

        match self.adj.get(&vi) {
            None => 0,
            Some(h) => h.len(),
        }
    }

    /// Panics if v not in the graph.
    pub fn degree_in(&self, v: &V) -> usize {
        let vi = self.vtoi[v];
        self.adj.values().filter(|h| h.contains_key(&vi)).count()
    }

    pub fn iter_vertices(&self) -> impl Iterator<Item = &V> + '_ {
        self.itov.values()
    }

    /// Panics if v not in the graph.
    pub fn iter_targets(&self, v: &V) -> impl Iterator<Item = (&E, &V)> + '_ {
        let vi = self.vtoi[v];

        match self.adj.get(&vi) {
            None => Box::new(std::iter::empty()) as Box<dyn Iterator<Item = (&E, &V)>>,
            Some(h) => Box::new(h.iter().map(|(vi, ei)| (&self.itoe[ei], &self.itov[vi]))),
        }
    }

    /// Panics if v not in the graph.
    pub fn iter_sources(&self, v: &V) -> impl Iterator<Item = (&E, &V)> + '_ {
        let vi = self.vtoi[v];
        self.adj.iter().flat_map(move |(svi, h)| {
            h.iter().filter_map(move |(dvi, ei)| {
                if *dvi == vi {
                    Some((&self.itoe[ei], &self.itov[svi]))
                } else {
                    None
                }
            })
        })
    }

    /// Panics if v1, v2 not in the graph.
    pub fn merge_vertices(&mut self, v1: &V, v2: &V) {
        let vi1 = self.vtoi[v1];
        let vi2 = self.vtoi[v2];
        if vi1 == vi2 {
            return;
        }

        // Migrate v2's out-edges
        let h2 = self.adj.remove(&vi2);
        let h1 = self.adj.entry(vi1).or_default();
        if let Some(h2) = h2 {
            for (vi, ei) in h2 {
                if let Entry::Vacant(e) = h1.entry(vi) {
                    e.insert(ei);
                } else {
                    // Remove would-be-duplicate edge
                    let e = &self.itoe[&ei];
                    self.etoi.remove(e);
                    self.itoe.remove(&ei);
                }
            }
        }

        // Migrate v2's in-edges
        for h in self.adj.values_mut() {
            if let Some(ei) = h.remove(&vi2) {
                if let Entry::Vacant(e) = h.entry(vi1) {
                    e.insert(ei);
                } else {
                    // Remove would-be-duplicate edge
                    let e = &self.itoe[&ei];
                    self.etoi.remove(e);
                    self.itoe.remove(&ei);
                }
            }
        }

        // Remove v2
        self.vtoi.remove(v2);
        self.itov.remove(&vi2);
    }

    pub fn has_vertex(&self, v: &V) -> bool {
        self.vtoi.contains_key(v)
    }

    pub fn has_edge(&self, v1: &V, v2: &V) -> bool {
        self.get_edge(v1, v2).is_some()
    }

    pub fn get_edge(&self, v1: &V, v2: &V) -> Option<&E> {
        let vi1 = self.vtoi.get(v1)?;
        let vi2 = self.vtoi.get(v2)?;
        let ei = self.adj.get(vi1)?.get(vi2)?;
        self.itoe.get(ei)
    }

    pub fn assert_invariant() {
        // TODO
    }

    pub fn save_to_file(&self, filename: &str) -> Result<(), String> {
        let f = match fs::File::create(filename) {
            Ok(f) => f,
            Err(e) => return Err(format!("open {}: {}", filename, e)),
        };
        match serde_json::to_writer(f, self) {
            Ok(()) => Ok(()),
            Err(e) => Err(format!("serialize: {}", e)),
        }
    }

    pub fn load_from_file(filename: &str) -> Result<Self, String> {
        let f = match fs::File::open(filename) {
            Ok(f) => f,
            Err(e) => return Err(format!("open {}: {}", filename, e)),
        };
        match serde_json::from_reader(f) {
            Ok(g) => Ok(g),
            Err(e) => Err(format!("deserialize: {}", e)),
        }
    }
}
