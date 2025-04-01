//! Hierarchical Interval Graph (HIG)
//!
//! This module implements a Hierarchical Interval Graph (HIG), which is a specialized graph structure
//! for representing hierarchical relationships between elements with interval semantics.
//!
//! # Core Concepts
//!
//! - **Hierarchical Identifier (HierarchicalId)**: A dot-separated sequence of numbers (e.g., "1.2.3")
//!   that uniquely identifies a position in a hierarchy. Components are parsed as unsigned integers.
//!
//! - **Vertex**: A node in the graph with a hierarchical ID and associated value. Vertices maintain
//!   references to their incoming and outgoing edges.
//!
//! - **Edge**: A directed connection between two vertices, optionally carrying metadata.
//!
//! - **Interval Semantics**: The graph supports interval-based queries where an interval is defined
//!   by two hierarchical IDs. This allows efficient querying of ranges within the hierarchy.
//!
//! # Key Features
//!
//! - Hierarchical organization with parent-child relationships
//! - Ancestor/descendant queries
//! - Interval-based queries
//! - Topological sorting
//! - Cycle prevention
//!
//! # Implementation Details
//!
//! The graph uses an arena-based approach for storing vertices and edges, with indices used as
//! references between components. This provides efficient memory usage and traversal operations.
//! When adding edges, the graph automatically maintains hierarchical relationships and prevents
//! the creation of cycles.


use std::collections::{HashMap, HashSet};
use std::cmp::Ordering;
use std::fmt;
/// A hierarchical identifier (e.g., "1.2.3")
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HierarchicalId {
    /// The components of the hierarchical ID (e.g., [1, 2, 3])
    components: Vec<usize>,
}

impl HierarchicalId {
    pub fn new(id_str: &str) -> Self {
        let components = id_str
            .split('.')
            .filter_map(|s| s.parse::<usize>().ok())
            .collect();
        Self { components }
    }
    pub fn from_components(components: Vec<usize>) -> Self {
        Self { components }
    }

    pub fn parent(&self) -> Option<Self> {
        if self.components.is_empty() {
            None
        } else {
            let mut parent_components = self.components.clone();
            parent_components.pop();
            if parent_components.is_empty() {
                None
            } else {
                Some(Self { components: parent_components })
            }
        }
    }

    pub fn is_ancestor_of(&self, other: &HierarchicalId) -> bool {
        if self.components.len() >= other.components.len() {
            return false;
        }
        
        for (i, component) in self.components.iter().enumerate() {
            if component != &other.components[i] {
                return false;
            }
        }
        true
    }

    pub fn is_descendant_of(&self, other: &HierarchicalId) -> bool {
        other.is_ancestor_of(self)
    }

    pub fn depth(&self) -> usize {
        self.components.len()
    }
    
    pub fn is_direct_child_of(&self, parent: &HierarchicalId) -> bool {
        if self.components.len() != parent.components.len() + 1 {
            return false;
        }
        
        for (i, component) in parent.components.iter().enumerate() {
            if component != &self.components[i] {
                return false;
            }
        }
        
        true
    }
    
    pub fn create_child(&self, component: usize) -> Self {
        let mut new_components = self.components.clone();
        new_components.push(component);
        Self { components: new_components }
    }
}

impl Ord for HierarchicalId {
    fn cmp(&self, other: &Self) -> Ordering {
        for (a, b) in self.components.iter().zip(other.components.iter()) {
            match a.cmp(b) {
                Ordering::Equal => continue,
                other_ordering => return other_ordering,
            }
        }
        
        self.components.len().cmp(&other.components.len())
    }
}

impl PartialOrd for HierarchicalId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for HierarchicalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.components
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(".")
        )
    }
}

pub type VertexIndex = usize;
pub type EdgeIndex = usize;

#[derive(Debug, Clone)]
pub struct Vertex<T> {
    id: HierarchicalId,
    value: Option<T>,
    outgoing: Vec<EdgeIndex>,
    incoming: Vec<EdgeIndex>,
}

impl<T> Vertex<T> {
    /// Create a new vertex
    pub fn new(id: HierarchicalId, value: Option<T>) -> Self {
        Self { 
            id, 
            value, 
            outgoing: Vec::new(),
            incoming: Vec::new(),
        }
    }

    pub fn id(&self) -> &HierarchicalId {
        &self.id
    }

    pub fn value(&self) -> Option<&T> {
        self.value.as_ref()
    }

    pub fn value_mut(&mut self) -> Option<&mut T> {
        self.value.as_mut()
    }
    
    pub fn set_value(&mut self, value: Option<T>) {
        self.value = value;
    }
    
    pub fn add_outgoing(&mut self, edge_idx: EdgeIndex) {
        self.outgoing.push(edge_idx);
    }
    
    pub fn add_incoming(&mut self, edge_idx: EdgeIndex) {
        self.incoming.push(edge_idx);
    }
    
    pub fn outgoing(&self) -> &[EdgeIndex] {
        &self.outgoing
    }
    
    pub fn incoming(&self) -> &[EdgeIndex] {
        &self.incoming
    }
}

#[derive(Debug, Clone)]
pub struct Edge<E> {
    source: VertexIndex,
    target: VertexIndex,
    value: Option<E>,
    metadata: Option<E>,
}

impl<E> Edge<E> {
    pub fn new(source: VertexIndex, target: VertexIndex, metadata: Option<E>) -> Self {
        Self { source, target, value: None, metadata }
    }
    
    pub fn new_with_value(source: VertexIndex, target: VertexIndex, value: Option<E>, metadata: Option<E>) -> Self {
        Self { source, target, value, metadata }
    }

    pub fn source(&self) -> VertexIndex {
        self.source
    }

    pub fn target(&self) -> VertexIndex {
        self.target
    }
    
    pub fn value(&self) -> Option<&E> {
        self.value.as_ref()
    }
    
    pub fn set_value(&mut self, value: Option<E>) {
        self.value = value;
    }

    pub fn metadata(&self) -> Option<&E> {
        self.metadata.as_ref()
    }
}

#[derive(Debug)]
pub struct HierarchicalIntervalGraph<V, E> {
    vertices: Vec<Vertex<V>>,
    edges: Vec<Edge<E>>,
    id_to_vertex: HashMap<HierarchicalId, VertexIndex>,
}

impl<V, E> Default for HierarchicalIntervalGraph<V, E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, E> HierarchicalIntervalGraph<V, E> {
    pub fn new() -> Self {
        Self {
            vertices: Vec::new(),
            edges: Vec::new(),
            id_to_vertex: HashMap::new(),
        }
    }

    pub fn add_vertex(&mut self, id: HierarchicalId, value: Option<V>) -> Result<VertexIndex, String> {
        if self.id_to_vertex.contains_key(&id) {
            return Err(format!("Vertex with ID {} already exists", id));
        }
        
        let vertex_idx = self.vertices.len();
        self.vertices.push(Vertex::new(id.clone(), value));
        self.id_to_vertex.insert(id, vertex_idx);
        
        Ok(vertex_idx)
    }

    pub fn add_edge(&mut self, source_id: &HierarchicalId, target_id: &HierarchicalId, metadata: Option<E>) -> Result<EdgeIndex, String> {
        self.add_edge_with_value(source_id, target_id, None, metadata)
    }
    
    pub fn add_edge_with_value(&mut self, source_id: &HierarchicalId, target_id: &HierarchicalId, value: Option<E>, metadata: Option<E>) -> Result<EdgeIndex, String> {
        let source_idx = self.get_vertex_idx(source_id)
            .ok_or_else(|| format!("Source vertex with ID {} does not exist", source_id))?;
        let target_idx = self.get_vertex_idx(target_id)
            .ok_or_else(|| format!("Target vertex with ID {} does not exist", target_id))?;
        let source_vertex = &self.vertices[source_idx];
        let target_vertex = &self.vertices[target_idx];
        if target_vertex.id().is_ancestor_of(source_vertex.id()) {
            return Err(format!("Adding edge from {} to {} would create a cycle", source_id, target_id));
        }
        
        let closest_edge_idx = self.find_closest_edge(source_id);
        
        let edge_idx = self.edges.len();
        self.edges.push(Edge::new_with_value(source_idx, target_idx, value, metadata));
        
        self.vertices[source_idx].add_outgoing(edge_idx);
        self.vertices[target_idx].add_incoming(edge_idx);
        
        if let Some(closest_idx) = closest_edge_idx {
            let closest_edge = &self.edges[closest_idx];
            let closest_source_idx = closest_edge.source();
            let closest_target_idx = closest_edge.target();
            
            let closest_source_id = self.vertices[closest_source_idx].id().clone();
            let closest_target_id = self.vertices[closest_target_idx].id().clone();
            
            if let Some(parent_id) = closest_source_id.parent() {
                if let Some(parent_idx) = self.get_vertex_idx(&parent_id) {
                    let parent_vertex = &self.vertices[parent_idx];
                    let source_vertex = &self.vertices[source_idx];
                    
                    if !source_vertex.id().is_ancestor_of(parent_vertex.id()) {
                        let parent_edge_idx = self.edges.len();
                        self.edges.push(Edge::new_with_value(parent_idx, source_idx, None, None));
                        
                        self.vertices[parent_idx].add_outgoing(parent_edge_idx);
                        self.vertices[source_idx].add_incoming(parent_edge_idx);
                    }
                }
            }
            
            let target_vertex = &self.vertices[target_idx];
            let closest_target_vertex = &self.vertices[closest_target_idx];
            
            if !closest_target_vertex.id().is_ancestor_of(target_vertex.id()) {
                let target_edge_idx = self.edges.len();
                self.edges.push(Edge::new_with_value(target_idx, closest_target_idx, None, None));
                
                self.vertices[target_idx].add_outgoing(target_edge_idx);
                self.vertices[closest_target_idx].add_incoming(target_edge_idx);
            }
        }
        
        Ok(edge_idx)
    }

    fn find_closest_edge(&self, id: &HierarchicalId) -> Option<EdgeIndex> {
        if self.edges.is_empty() {
            return None;
        }
        
        self.edges
            .iter()
            .enumerate()
            .min_by(|(_, a), (_, b)| {
                let a_source_id = self.vertices[a.source()].id();
                let b_source_id = self.vertices[b.source()].id();
                
                let a_dist = self.hierarchical_distance(a_source_id, id);
                let b_dist = self.hierarchical_distance(b_source_id, id);
                a_dist.cmp(&b_dist)
            })
            .map(|(idx, _)| idx)
    }

    fn hierarchical_distance(&self, a: &HierarchicalId, b: &HierarchicalId) -> usize {
        let min_len = a.depth().min(b.depth());
        let mut diff_count = a.depth().abs_diff(b.depth());
        
        for i in 0..min_len {
            if a.components[i] != b.components[i] {
                diff_count += 1;
            }
        }
        
        diff_count
    }

    pub fn get_vertex_idx(&self, id: &HierarchicalId) -> Option<VertexIndex> {
        self.id_to_vertex.get(id).copied()
    }

    pub fn get_vertex(&self, idx: VertexIndex) -> Option<&Vertex<V>> {
        self.vertices.get(idx)
    }

    pub fn get_vertex_by_id(&self, id: &HierarchicalId) -> Option<&Vertex<V>> {
        self.get_vertex_idx(id).and_then(|idx| self.get_vertex(idx))
    }

    pub fn get_vertex_mut(&mut self, idx: VertexIndex) -> Option<&mut Vertex<V>> {
        self.vertices.get_mut(idx)
    }

    pub fn get_vertex_by_id_mut(&mut self, id: &HierarchicalId) -> Option<&mut Vertex<V>> {
        self.get_vertex_idx(id).and_then(move |idx| self.get_vertex_mut(idx))
    }

    pub fn get_edge(&self, idx: EdgeIndex) -> Option<&Edge<E>> {
        self.edges.get(idx)
    }

    pub fn vertices(&self) -> impl Iterator<Item = &Vertex<V>> {
        self.vertices.iter()
    }

    pub fn edges(&self) -> impl Iterator<Item = &Edge<E>> {
        self.edges.iter()
    }

    pub fn outgoing_edges(&self, id: &HierarchicalId) -> Vec<&Edge<E>> {
        if let Some(vertex_idx) = self.get_vertex_idx(id) {
            self.vertices[vertex_idx]
                .outgoing()
                .iter()
                .filter_map(|&edge_idx| self.edges.get(edge_idx))
                .collect()
        } else {
            Vec::new()
        }
    }

    pub fn incoming_edges(&self, id: &HierarchicalId) -> Vec<&Edge<E>> {
        if let Some(vertex_idx) = self.get_vertex_idx(id) {
            self.vertices[vertex_idx]
                .incoming()
                .iter()
                .filter_map(|&edge_idx| self.edges.get(edge_idx))
                .collect()
        } else {
            Vec::new()
        }
    }

    pub fn id_in_edge_interval(&self, id: &HierarchicalId, edge_idx: EdgeIndex) -> bool {
        if let Some(edge) = self.get_edge(edge_idx) {
            let source_id = self.vertices[edge.source()].id();
            let target_id = self.vertices[edge.target()].id();
            
            source_id <= id && id <= target_id
        } else {
            false
        }
    }

    pub fn topological_sort(&self) -> Result<Vec<VertexIndex>, String> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();
        
        fn visit<V, E>(
            graph: &HierarchicalIntervalGraph<V, E>,
            idx: VertexIndex,
            visited: &mut HashSet<VertexIndex>,
            temp_visited: &mut HashSet<VertexIndex>,
            result: &mut Vec<VertexIndex>,
        ) -> Result<(), String> {
            if temp_visited.contains(&idx) {
                return Err("Graph contains a cycle".to_string());
            }
            
            if !visited.contains(&idx) {
                temp_visited.insert(idx);
                
                for &edge_idx in graph.vertices[idx].outgoing() {
                    if let Some(edge) = graph.get_edge(edge_idx) {
                        visit(graph, edge.target(), visited, temp_visited, result)?;
                    }
                }
                
                temp_visited.remove(&idx);
                visited.insert(idx);
                result.push(idx);
            }
            
            Ok(())
        }
        
        for idx in 0..self.vertices.len() {
            if !visited.contains(&idx) {
                visit(self, idx, &mut visited, &mut temp_visited, &mut result)?;
            }
        }
        
        result.reverse();
        Ok(result)
    }
    
    /// Performs a topological sort and returns edges in topological order
    ///
    /// This method returns a vector of edge indices that respect the topological ordering
    /// of the vertices. For each vertex in the topological order, all its outgoing edges
    /// are included in the result.
    pub fn topological_sort_edges(&self) -> Result<Vec<EdgeIndex>, String> {
        // First get vertices in topological order
        let sorted_vertices = self.topological_sort()?;
        let mut result = Vec::new();
        let mut visited_edges = HashSet::new();
        
        // For each vertex in topological order
        for &vertex_idx in &sorted_vertices {
            // Add all outgoing edges from this vertex
            for &edge_idx in self.vertices[vertex_idx].outgoing() {
                if !visited_edges.contains(&edge_idx) {
                    result.push(edge_idx);
                    visited_edges.insert(edge_idx);
                }
            }
        }
        
        Ok(result)
    }

    pub fn query_interval(&self, start: &HierarchicalId, end: &HierarchicalId) -> Vec<VertexIndex> {
        (0..self.vertices.len())
            .filter(|&idx| {
                let id = self.vertices[idx].id();
                id.is_direct_child_of(start) && id < end
            })
            .collect()
    }

    pub fn query_descendants(&self, ancestor: &HierarchicalId) -> Vec<VertexIndex> {
        (0..self.vertices.len())
            .filter(|&idx| {
                let id = self.vertices[idx].id();
                ancestor != id && id.is_descendant_of(ancestor)
            })
            .collect()
    }

    pub fn query_ancestors(&self, descendant: &HierarchicalId) -> Vec<VertexIndex> {
        (0..self.vertices.len())
            .filter(|&idx| {
                let id = self.vertices[idx].id();
                id.is_ancestor_of(descendant)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_hierarchical_id() {
        let id = HierarchicalId::new("1.2.3");
        assert_eq!(id.to_string(), "1.2.3");
        
        let parent = id.parent().unwrap();
        assert_eq!(parent.to_string(), "1.2");
        
        let child = id.create_child(4);
        assert_eq!(child.to_string(), "1.2.3.4");
        
        assert!(parent.is_ancestor_of(&id));
        assert!(id.is_descendant_of(&parent));
    }

    #[test]
    fn test_add_vertex_and_edge() {
        let mut graph = HierarchicalIntervalGraph::<&str, ()>::new();
        
        let root_idx = graph.add_vertex(HierarchicalId::new("1"), Some("Root")).unwrap();
        let child1_idx = graph.add_vertex(HierarchicalId::new("1.1"), Some("Child 1")).unwrap();
        let child2_idx = graph.add_vertex(HierarchicalId::new("1.2"), Some("Child 2")).unwrap();
        let grandchild_idx = graph.add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1")).unwrap();
        
        graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), None).unwrap();
        graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), None).unwrap();
        graph.add_edge(&HierarchicalId::new("1.1"), &HierarchicalId::new("1.1.1"), None).unwrap();
        
        assert_eq!(graph.vertices().count(), 4);
        
        assert!(graph.edges().count() >= 3);

        
        let outgoing = graph.outgoing_edges(&HierarchicalId::new("1"));
        assert!(outgoing.len() >= 2);
        
        let incoming = graph.incoming_edges(&HierarchicalId::new("1.1"));
        assert!(!incoming.is_empty());
    }

#[test]
    fn test_topological_sort() {
        let mut graph = HierarchicalIntervalGraph::<&str, ()>::new();
        
        let root_idx = graph.add_vertex(HierarchicalId::new("1"), Some("Root")).unwrap();
        let child1_idx = graph.add_vertex(HierarchicalId::new("1.1"), Some("Child 1")).unwrap();
        let child2_idx = graph.add_vertex(HierarchicalId::new("1.2"), Some("Child 2")).unwrap();
        let grandchild_idx = graph.add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1")).unwrap();
        
        let edge1_idx = graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), None).unwrap();
        let edge2_idx = graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), None).unwrap();
        let edge3_idx = graph.add_edge(&HierarchicalId::new("1.1"), &HierarchicalId::new("1.1.1"), None).unwrap();
        
        let sorted = graph.topological_sort().unwrap();
        
        assert_eq!(sorted[0], root_idx);
        
        assert!(sorted.contains(&grandchild_idx));
    }
    
#[test]
    fn test_topological_sort_edges() {
        let mut graph = HierarchicalIntervalGraph::<&str, ()>::new();
        
        // Create a simple graph
        graph.add_vertex(HierarchicalId::new("1"), Some("Root")).unwrap();
        graph.add_vertex(HierarchicalId::new("1.1"), Some("Child 1")).unwrap();
        graph.add_vertex(HierarchicalId::new("1.2"), Some("Child 2")).unwrap();
        graph.add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1")).unwrap();
        
        let edge1_idx = graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), None).unwrap();
        let edge2_idx = graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), None).unwrap();
        let edge3_idx = graph.add_edge(&HierarchicalId::new("1.1"), &HierarchicalId::new("1.1.1"), None).unwrap();
        
        let sorted_edges = graph.topological_sort_edges().unwrap();
        
        // Verify that edges appear in topological order
        // Edge from root to children should come before edge from child to grandchild
        assert!(sorted_edges.contains(&edge1_idx));
        assert!(sorted_edges.contains(&edge2_idx));
        assert!(sorted_edges.contains(&edge3_idx));
        
        // Find positions of each edge
        let pos1 = sorted_edges.iter().position(|&e| e == edge1_idx).unwrap();
        let pos3 = sorted_edges.iter().position(|&e| e == edge3_idx).unwrap();
        
        // Edge from parent to child should come before edge from child to grandchild
        assert!(pos1 < pos3);
    }

#[test]
    fn test_query_interval() {
        let mut graph = HierarchicalIntervalGraph::<&str, ()>::new();
        
        let root_idx = graph.add_vertex(HierarchicalId::new("1"), Some("Root")).unwrap();
        let child1_idx = graph.add_vertex(HierarchicalId::new("1.1"), Some("Child 1")).unwrap();
        let child2_idx = graph.add_vertex(HierarchicalId::new("1.2"), Some("Child 2")).unwrap();
        let grandchild_idx = graph.add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1")).unwrap();
        let another_root_idx = graph.add_vertex(HierarchicalId::new("2"), Some("Another Root")).unwrap();
        
        let results = graph.query_interval(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"));
        assert_eq!(results.len(), 1);

        
        let descendants = graph.query_descendants(&HierarchicalId::new("1"));
        assert_eq!(descendants.len(), 3);

        
        let ancestors = graph.query_ancestors(&HierarchicalId::new("1.1.1"));
        assert_eq!(ancestors.len(), 2);

    }

#[cfg(test)]
mod dot_tests {
    use super::*;
    use crate::hig_dot::HigToDot;
    
    #[test]
    fn test_hig_to_dot() {
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();
        
        graph.add_vertex(HierarchicalId::new("1"), Some("Root")).unwrap();
        graph.add_vertex(HierarchicalId::new("1.1"), Some("Child 1")).unwrap();
        graph.add_vertex(HierarchicalId::new("1.2"), Some("Child 2")).unwrap();
        graph.add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1")).unwrap();
        
        graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), Some("Edge 1")).unwrap();
        graph.add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), Some("Edge 2")).unwrap();
        graph.add_edge(&HierarchicalId::new("1.1"), &HierarchicalId::new("1.1.1"), Some("Edge 3")).unwrap();
        
        let dot_output = graph.to_dot("TestGraph");
        
        // Basic validation
        assert!(dot_output.starts_with("digraph TestGraph {"));
        assert!(dot_output.contains("n0 [label=\"1\", tooltip=\"Root\"];"));
        assert!(dot_output.contains("n0 -> n1"));
        assert!(dot_output.ends_with("}
"));
    }
}
}
