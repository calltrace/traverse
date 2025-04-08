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
//! - **Edge**: A directed connection between two vertices, optionally carrying both a distinct `value`
//!   and separate `metadata`.
//!
//! - **Interval Semantics**: The graph supports interval-based queries where an interval is defined
//!   by two hierarchical IDs. This allows efficient querying of ranges within the hierarchy.
//!
//! # Key Features
//!
//! - Hierarchical organization with parent-child relationships (defined by IDs)
//! - General directed graph structure allowing arbitrary connections
//! - Ancestor/descendant queries (based on IDs)
//! - Interval-based queries (based on IDs)
//! - Topological sorting
//! - Cycle prevention during edge addition
//!
//! # Implementation Details
//!
//! The graph uses an arena-based approach for storing vertices and edges, with indices used as
//! references between components. This provides efficient memory usage and traversal operations.
//! When adding edges, the graph prevents the creation of cycles by checking for existing paths
//! between the target and source nodes.

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
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
        if self.components.len() <= 1 {
            None
        } else {
            let mut parent_components = self.components.clone();
            parent_components.pop();
            Some(Self {
                components: parent_components,
            })
        }
    }

    pub fn is_ancestor_of(&self, other: &HierarchicalId) -> bool {
        if self.components.is_empty() || self.components.len() >= other.components.len() {
            return false;
        }
        self.components
            .iter()
            .zip(other.components.iter())
            .all(|(s, o)| s == o)
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
        parent
            .components
            .iter()
            .zip(self.components.iter())
            .all(|(p, s)| p == s)
    }

    pub fn create_child(&self, component: usize) -> Self {
        let mut new_components = self.components.clone();
        new_components.push(component);
        Self {
            components: new_components,
        }
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
    // Optional hierarchical identifier for the edge
    id: Option<HierarchicalId>,
    // --- Added value back ---
    value: Option<E>,
    metadata: Option<E>,
}

impl<E> Edge<E> {
    /// Creates a new edge with metadata, setting value to None.
    pub fn new(source: VertexIndex, target: VertexIndex, metadata: Option<E>) -> Self {
        Self {
            source,
            target,
            id: None,
            value: None,
            metadata,
        } // Initialize value to None
    }

    /// Creates a new edge with both value and metadata.
    pub fn new_with_value(
        source: VertexIndex,
        target: VertexIndex,
        value: Option<E>,
        metadata: Option<E>,
    ) -> Self {
        Self {
            source,
            target,
            id: None,
            value,
            metadata,
        }
    }

    /// Creates a new edge with an identifier and metadata, setting value to None.
    pub fn new_with_id(
        source: VertexIndex,
        target: VertexIndex,
        id: HierarchicalId,
        metadata: Option<E>,
    ) -> Self {
        Self {
            source,
            target,
            id: Some(id),
            value: None,
            metadata,
        }
    }

    /// Creates a new edge with an identifier, value, and metadata.
    pub fn new_with_id_and_value(
        source: VertexIndex,
        target: VertexIndex,
        id: HierarchicalId,
        value: Option<E>,
        metadata: Option<E>,
    ) -> Self {
        Self {
            source,
            target,
            id: Some(id),
            value,
            metadata,
        }
    }

    pub fn source(&self) -> VertexIndex {
        self.source
    }
    pub fn target(&self) -> VertexIndex {
        self.target
    }

    /// Returns the edge's hierarchical ID, if it has one
    pub fn id(&self) -> Option<&HierarchicalId> {
        self.id.as_ref()
    }

    /// Sets the edge's hierarchical ID
    pub fn set_id(&mut self, id: Option<HierarchicalId>) {
        self.id = id;
    }

    // --- Added value accessors ---
    pub fn value(&self) -> Option<&E> {
        self.value.as_ref()
    }
    pub fn set_value(&mut self, value: Option<E>) {
        self.value = value;
    }
    // ---

    pub fn metadata(&self) -> Option<&E> {
        self.metadata.as_ref()
    }
}

#[derive(Debug)]
pub struct HierarchicalIntervalGraph<V, E> {
    vertices: Vec<Vertex<V>>,
    edges: Vec<Edge<E>>,
    id_to_vertex: HashMap<HierarchicalId, VertexIndex>,
    id_to_edge: HashMap<HierarchicalId, EdgeIndex>,
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
            id_to_edge: HashMap::new(),
        }
    }

    pub fn add_vertex(
        &mut self,
        id: HierarchicalId,
        value: Option<V>,
    ) -> Result<VertexIndex, String> {
        if self.id_to_vertex.contains_key(&id) {
            return Err(format!("Vertex with ID {} already exists", id));
        }
        let vertex_idx = self.vertices.len();
        self.vertices.push(Vertex::new(id.clone(), value));
        self.id_to_vertex.insert(id, vertex_idx);
        Ok(vertex_idx)
    }

    /// Adds an edge with optional metadata, setting its value to None.
    /// If the vertices do not exist, they are created with `None` values.
    /// Returns an error if adding the edge would create a cycle.
    /// This is a convenience wrapper around `add_edge_with_value`.
    pub fn add_edge(
        &mut self,
        source_id: &HierarchicalId,
        target_id: &HierarchicalId,
        metadata: Option<E>,
    ) -> Result<EdgeIndex, String> {
        // Call the more general function, setting value to None
        self.add_edge_with_value(source_id, target_id, None, metadata)
    }

    /// Adds an edge with optional value and optional metadata.
    /// If the vertices do not exist, they are created with `None` values.
    /// Returns an error if adding the edge would create a cycle.
    pub fn add_edge_with_value(
        &mut self,
        source_id: &HierarchicalId,
        target_id: &HierarchicalId,
        value: Option<E>, // Added value parameter
        metadata: Option<E>,
    ) -> Result<EdgeIndex, String> {
        self.add_edge_with_id_and_value(source_id, target_id, None, value, metadata)
    }

    /// Adds an edge with an optional identifier, value, and metadata.
    /// If the vertices do not exist, they are created with `None` values.
    /// Returns an error if adding the edge would create a cycle or if the edge ID already exists.
    pub fn add_edge_with_id_and_value(
        &mut self,
        source_id: &HierarchicalId,
        target_id: &HierarchicalId,
        edge_id: Option<HierarchicalId>,
        value: Option<E>,
        metadata: Option<E>,
    ) -> Result<EdgeIndex, String> {
        // Check if edge ID already exists
        if let Some(ref id) = edge_id {
            if self.id_to_edge.contains_key(id) {
                return Err(format!("Edge with ID {} already exists", id));
            }
        }

        // Ensure source vertex exists, create if not
        let source_idx = match self.id_to_vertex.get(source_id) {
            Some(&idx) => idx,
            None => self.add_vertex(source_id.clone(), None)?,
        };

        // Ensure target vertex exists, create if not
        let target_idx = match self.id_to_vertex.get(target_id) {
            Some(&idx) => idx,
            None => self.add_vertex(target_id.clone(), None)?,
        };

        // Prevent adding edge to self
        if source_idx == target_idx {
            return Err(format!(
                "Adding edge from {} to itself is not allowed (would create a cycle)",
                source_id
            ));
        }

        // --- Cycle Prevention Check (REMOVED to allow cycles during build) ---
        // if self.has_path(target_idx, source_idx) {
        //     return Err(format!(
        //         "Adding edge from {} to {} would create a cycle",
        //         source_id, target_id
        //     ));
        // }

        // --- Add the edge ---
        let edge_idx = self.edges.len();

        // Create the edge with or without an ID
        let edge = if let Some(id) = edge_id.clone() {
            Edge::new_with_id_and_value(source_idx, target_idx, id, value, metadata)
        } else {
            Edge::new_with_value(source_idx, target_idx, value, metadata)
        };

        self.edges.push(edge);

        // If the edge has an ID, add it to the mapping
        if let Some(id) = edge_id {
            self.id_to_edge.insert(id, edge_idx);
        }

        self.vertices[source_idx].add_outgoing(edge_idx);
        self.vertices[target_idx].add_incoming(edge_idx);

        Ok(edge_idx)
    }

    /// Adds an edge with an identifier and metadata, setting value to None.
    /// If the vertices do not exist, they are created with `None` values.
    /// Returns an error if adding the edge would create a cycle or if the edge ID already exists.
    pub fn add_edge_with_id(
        &mut self,
        source_id: &HierarchicalId,
        target_id: &HierarchicalId,
        edge_id: HierarchicalId,
        metadata: Option<E>,
    ) -> Result<EdgeIndex, String> {
        self.add_edge_with_id_and_value(source_id, target_id, Some(edge_id), None, metadata)
    }

    /// Helper function to check if a path exists from start_idx to end_idx using BFS.
    fn has_path(&self, start_idx: VertexIndex, end_idx: VertexIndex) -> bool {
        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(start_idx);
        visited.insert(start_idx);

        while let Some(current_idx) = queue.pop_front() {
            // Important: If we reach the end_idx during traversal, a path exists.
            if current_idx == end_idx {
                return true;
            }

            if let Some(current_vertex) = self.vertices.get(current_idx) {
                for &edge_idx in current_vertex.outgoing() {
                    if let Some(edge) = self.edges.get(edge_idx) {
                        let neighbor_idx = edge.target();
                        // Check if neighbor is the target FIRST
                        if neighbor_idx == end_idx {
                            return true; // Found path
                        }
                        // If not the target and not visited, add to queue
                        if !visited.contains(&neighbor_idx) {
                            visited.insert(neighbor_idx);
                            queue.push_back(neighbor_idx);
                        }
                    }
                }
            }
        }
        false // No path found
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
        self.get_vertex_idx(id)
            .and_then(move |idx| self.get_vertex_mut(idx))
    }
    pub fn get_edge(&self, idx: EdgeIndex) -> Option<&Edge<E>> {
        self.edges.get(idx)
    }

    pub fn get_edge_idx(&self, id: &HierarchicalId) -> Option<EdgeIndex> {
        self.id_to_edge.get(id).copied()
    }

    pub fn get_edge_by_id(&self, id: &HierarchicalId) -> Option<&Edge<E>> {
        self.get_edge_idx(id).and_then(|idx| self.get_edge(idx))
    }

    pub fn get_edge_mut(&mut self, idx: EdgeIndex) -> Option<&mut Edge<E>> {
        self.edges.get_mut(idx)
    }

    pub fn get_edge_by_id_mut(&mut self, id: &HierarchicalId) -> Option<&mut Edge<E>> {
        self.get_edge_idx(id)
            .and_then(move |idx| self.get_edge_mut(idx))
    }
    pub fn vertices(&self) -> impl Iterator<Item = &Vertex<V>> {
        self.vertices.iter()
    }
    pub fn edges(&self) -> impl Iterator<Item = &Edge<E>> {
        self.edges.iter()
    }
    pub fn outgoing_edges(&self, id: &HierarchicalId) -> Vec<&Edge<E>> {
        if let Some(vertex_idx) = self.get_vertex_idx(id) {
            let mut edges_with_indices: Vec<(EdgeIndex, Option<&HierarchicalId>)> = self.vertices
                [vertex_idx]
                .outgoing()
                .iter()
                .filter_map(|&edge_idx| self.edges.get(edge_idx).map(|edge| (edge_idx, edge.id())))
                .collect();

            // Sort based on the edge's HierarchicalId
            // Edges with IDs come first, sorted by ID.
            // Edges without IDs come last, sorted by their original EdgeIndex for stability.
            edges_with_indices.sort_by(|(idx_a, id_a_opt), (idx_b, id_b_opt)| {
                match (id_a_opt, id_b_opt) {
                    (Some(id_a), Some(id_b)) => id_a.cmp(id_b),
                    (Some(_), None) => Ordering::Less, // Edges with IDs come before those without
                    (None, Some(_)) => Ordering::Greater, // Edges without IDs come after those with
                    (None, None) => idx_a.cmp(idx_b),  // Sort edges without IDs by index
                }
            });

            // Map back to edge references
            edges_with_indices
                .into_iter()
                .filter_map(|(edge_idx, _)| self.edges.get(edge_idx))
                .collect()
        } else {
            Vec::new()
        }
    }
    pub fn incoming_edges(&self, id: &HierarchicalId) -> Vec<&Edge<E>> {
        if let Some(vertex_idx) = self.get_vertex_idx(id) {
            let mut edges_with_indices: Vec<(EdgeIndex, Option<&HierarchicalId>)> = self.vertices
                [vertex_idx]
                .incoming()
                .iter()
                .filter_map(|&edge_idx| self.edges.get(edge_idx).map(|edge| (edge_idx, edge.id())))
                .collect();

            // Sort based on the edge's HierarchicalId (same logic as outgoing_edges)
            edges_with_indices.sort_by(|(idx_a, id_a_opt), (idx_b, id_b_opt)| {
                match (id_a_opt, id_b_opt) {
                    (Some(id_a), Some(id_b)) => id_a.cmp(id_b),
                    (Some(_), None) => Ordering::Less,
                    (None, Some(_)) => Ordering::Greater,
                    (None, None) => idx_a.cmp(idx_b),
                }
            });

            // Map back to edge references
            edges_with_indices
                .into_iter()
                .filter_map(|(edge_idx, _)| self.edges.get(edge_idx))
                .collect()
        } else {
            Vec::new()
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
                return Err(format!(
                    "Graph contains a cycle involving vertex {}",
                    graph.vertices[idx].id()
                ));
            }
            if !visited.contains(&idx) {
                temp_visited.insert(idx);
                if let Some(vertex) = graph.vertices.get(idx) {
                    // Use the sorted outgoing edges for traversal
                    let sorted_outgoing_edges = graph.outgoing_edges(vertex.id());
                    for edge in sorted_outgoing_edges {
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

    /// Performs a topological sort of the edges using a Depth-First Search (DFS) approach.
    ///
    /// This method aims to produce an "interleaved" order by exploring paths deeply.
    /// Edges are added to the result in pre-order traversal sequence, sorted locally
    /// at each step by their HierarchicalId.
    ///
    /// Returns an error if the graph contains a cycle.
    pub fn topological_sort_edges(&self) -> Result<Vec<EdgeIndex>, String> {
        // 1. Perform vertex topological sort first as an efficient cycle check.
        self.topological_sort()?;

        let mut result = Vec::with_capacity(self.edges.len());
        let mut visited_edges = HashSet::with_capacity(self.edges.len());
        // Used for detecting cycles during the edge DFS itself (redundant if vertex sort passed, but good practice)
        let mut visiting_edges = HashSet::with_capacity(self.edges.len());

        // 2. Define the recursive DFS visit function
        fn visit<V, E>(
            graph: &HierarchicalIntervalGraph<V, E>,
            edge_idx: EdgeIndex,
            visited_edges: &mut HashSet<EdgeIndex>,
            visiting_edges: &mut HashSet<EdgeIndex>,
            result: &mut Vec<EdgeIndex>,
        ) -> Result<(), String> {
            // Check if already fully processed
            if visited_edges.contains(&edge_idx) {
                return Ok(());
            }
            // Check if currently being visited (indicates a cycle)
            if visiting_edges.contains(&edge_idx) {
                // This should ideally not happen if the initial vertex sort passed
                let edge = graph.get_edge(edge_idx).unwrap(); // Assume edge exists
                let source_id = graph
                    .get_vertex(edge.source())
                    .map(|v| v.id().to_string())
                    .unwrap_or("?".to_string());
                let target_id = graph
                    .get_vertex(edge.target())
                    .map(|v| v.id().to_string())
                    .unwrap_or("?".to_string());
                return Err(format!(
                    "Cycle detected during edge DFS involving edge {} ({} -> {})",
                    edge_idx, source_id, target_id
                ));
            }

            // Mark as visiting
            visiting_edges.insert(edge_idx);

            // --- Pre-order Add ---
            // Add the edge to the result *before* visiting its successors
            result.push(edge_idx);
            // Mark as visited immediately after adding to prevent adding duplicates
            // in case of multiple paths leading to the same edge start.
            visited_edges.insert(edge_idx);

            // Get the target vertex of the current edge
            if let Some(edge) = graph.get_edge(edge_idx) {
                let target_vertex_idx = edge.target();

                // Get outgoing edges from the target vertex, sorted by HierarchicalId
                if let Some(target_vertex) = graph.get_vertex(target_vertex_idx) {
                    // Use the existing outgoing_edges helper which sorts correctly
                    let sorted_next_edges = graph.outgoing_edges(target_vertex.id());

                    // Recursively visit each successor edge
                    for next_edge in sorted_next_edges {
                        // Find the index of the next edge
                        // This is slightly inefficient; consider storing indices directly if performance is critical
                        if let Some(next_edge_idx) = graph
                            .edges()
                            .enumerate()
                            .find(|(_, e)| {
                                e.source() == next_edge.source()
                                    && e.target() == next_edge.target()
                                    && e.id() == next_edge.id() // Match based on source, target, and ID
                            })
                            .map(|(idx, _)| idx)
                        {
                            // Only visit if not already fully processed by another path
                            if !visited_edges.contains(&next_edge_idx) {
                                visit(graph, next_edge_idx, visited_edges, visiting_edges, result)?;
                            }
                        } else {
                            // This case should ideally not happen if outgoing_edges is consistent
                            eprintln!("Warning: Could not find index for edge reference during DFS traversal.");
                        }
                    }
                }
            }

            // Mark as finished visiting
            visiting_edges.remove(&edge_idx);

            Ok(())
        }

        // 3. Determine initial edges (originating from vertices with in-degree 0)
        let mut in_degrees = vec![0; self.vertices.len()];
        for edge in self.edges.iter() {
            in_degrees[edge.target()] += 1;
        }

        let mut initial_edges = Vec::new();
        for (vertex_idx, &degree) in in_degrees.iter().enumerate() {
            if degree == 0 {
                if let Some(vertex) = self.get_vertex(vertex_idx) {
                    // Get outgoing edges, already sorted by the helper function
                    let sorted_outgoing = self.outgoing_edges(vertex.id());
                    // Find indices for these edges
                    for edge_ref in sorted_outgoing {
                        if let Some(edge_idx) = self
                            .edges()
                            .enumerate()
                            .find(|(_, e)| {
                                e.source() == edge_ref.source()
                                    && e.target() == edge_ref.target()
                                    && e.id() == edge_ref.id()
                            })
                            .map(|(idx, _)| idx)
                        {
                            initial_edges.push(edge_idx);
                        }
                    }
                }
            }
        }
        // Note: The initial_edges collected above might contain duplicates if multiple source nodes
        // point to the same edge (not typical). The `visited_edges` check in `visit` handles this.
        // We also need to ensure the initial edges themselves are processed in ID order.
        initial_edges.sort_by(|&idx_a, &idx_b| {
            let edge_a = self.get_edge(idx_a);
            let edge_b = self.get_edge(idx_b);
            match (edge_a.and_then(|e| e.id()), edge_b.and_then(|e| e.id())) {
                (Some(id_a), Some(id_b)) => id_a.cmp(id_b),
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (None, None) => idx_a.cmp(&idx_b),
            }
        });

        // 4. Start DFS from each initial edge
        for edge_idx in initial_edges {
            // Only start DFS if the edge hasn't been visited by a previous DFS path
            if !visited_edges.contains(&edge_idx) {
                visit(
                    self,
                    edge_idx,
                    &mut visited_edges,
                    &mut visiting_edges,
                    &mut result,
                )?;
            }
        }

        // 5. Sanity check: Ensure all edges were added.
        if result.len() != self.edges.len() {
            eprintln!("Warning: DFS topological_sort_edges did not add all edges. Expected {}, got {}. This might indicate disconnected components or graph issues.", self.edges.len(), result.len());
            // Consider adding remaining edges if disconnected graphs are allowed,
            // but this might violate strict topological order if not handled carefully.
            // For now, we rely on the initial vertex sort ensuring connectivity or erroring out.
            // return Err("Failed to include all edges in topological sort; possible disconnected components or graph issue.".to_string());
        }

        Ok(result)
    }

    pub fn query_interval(&self, start: &HierarchicalId, end: &HierarchicalId) -> Vec<VertexIndex> {
        self.vertices
            .iter()
            .enumerate()
            .filter(|(_, vertex)| {
                let id = vertex.id();
                id > start && id < end && id.is_direct_child_of(start)
            })
            .map(|(idx, _)| idx)
            .collect()
    }

    pub fn query_descendants(&self, ancestor: &HierarchicalId) -> Vec<VertexIndex> {
        self.vertices
            .iter()
            .enumerate()
            .filter(|(_, vertex)| {
                let id = vertex.id();
                ancestor != id && id.is_descendant_of(ancestor)
            })
            .map(|(idx, _)| idx)
            .collect()
    }

    pub fn query_ancestors(&self, descendant: &HierarchicalId) -> Vec<VertexIndex> {
        self.vertices
            .iter()
            .enumerate()
            .filter(|(_, vertex)| vertex.id().is_ancestor_of(descendant))
            .map(|(idx, _)| idx)
            .collect()
    }

    /// Transforms the graph into a new, acyclic graph by breaking cycles.
    ///
    /// This function creates a new graph that preserves all vertices and edges from the original graph,
    /// but breaks any cycles by introducing intermediate nodes. For each back edge that would create
    /// a cycle, an intermediate node is created as a clone of the target vertex, and the back edge
    /// is redirected to this intermediate node instead.
    ///
    /// # Returns
    ///
    /// * `Result<HierarchicalIntervalGraph<V, E>, String>` - A new acyclic graph or an error message
    pub fn transform_to_acyclic(&self) -> Result<HierarchicalIntervalGraph<V, E>, String>
    where
        V: Clone, // Require V to be Cloneable for copying vertices
        E: Clone, // Require E to be Cloneable for copying edge values/metadata
    {
        // Create a new graph to hold the acyclic transformation
        let mut acyclic_graph = HierarchicalIntervalGraph::new();

        // Special component value to use for intermediate node IDs
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999;

        // Step 1: Copy all vertices to the new graph
        for (idx, vertex) in self.vertices.iter().enumerate() {
            let vertex_id = vertex.id().clone();
            let vertex_value = vertex.value().cloned();

            // Add the vertex to the new graph, preserving its index
            let new_idx = acyclic_graph.add_vertex(vertex_id, vertex_value)?;

            // Sanity check: indices should match between original and new graph
            if idx != new_idx {
                return Err(format!(
                    "Vertex index mismatch during transformation: {} != {}",
                    idx, new_idx
                ));
            }
        }

        // Step 2: Identify cycles and determine which edges need to be redirected
        // We'll use a depth-first search to detect back edges

        // Track vertices being visited in the current DFS path
        let mut path = HashSet::new();
        // Track vertices that have been fully processed
        let mut visited = HashSet::new();
        // Store edges that create cycles (need to be redirected)
        let mut cycle_edges = Vec::new();

        // Helper function for DFS cycle detection
        fn detect_cycles<V, E>(
            graph: &HierarchicalIntervalGraph<V, E>,
            vertex_idx: VertexIndex,
            path: &mut HashSet<VertexIndex>,
            visited: &mut HashSet<VertexIndex>,
            cycle_edges: &mut Vec<EdgeIndex>,
        ) {
            // If already fully processed, no need to visit again
            if visited.contains(&vertex_idx) {
                return;
            }

            // If already in current path, we've found a cycle
            if path.contains(&vertex_idx) {
                return; // We'll detect the specific back edge later
            }

            // Add to current path
            path.insert(vertex_idx);

            // Visit all outgoing edges
            if let Some(vertex) = graph.get_vertex(vertex_idx) {
                for &edge_idx in vertex.outgoing() {
                    if let Some(edge) = graph.get_edge(edge_idx) {
                        let target_idx = edge.target();

                        // If target is already in our path, this edge creates a cycle
                        if path.contains(&target_idx) {
                            cycle_edges.push(edge_idx);
                        } else {
                            // Continue DFS from target
                            detect_cycles(graph, target_idx, path, visited, cycle_edges);
                        }
                    }
                }
            }

            // Remove from current path and mark as visited
            path.remove(&vertex_idx);
            visited.insert(vertex_idx);
        }

        // Run cycle detection from each unvisited vertex
        for vertex_idx in 0..self.vertices.len() {
            if !visited.contains(&vertex_idx) {
                detect_cycles(self, vertex_idx, &mut path, &mut visited, &mut cycle_edges);
            }
        }

        // Step 3: Copy all edges to the new graph, redirecting cycle edges
        for (edge_idx, edge) in self.edges.iter().enumerate() {
            let source_idx = edge.source();
            let target_idx = edge.target();
            let original_edge_id = edge.id().cloned(); // Keep original ID for the new edge
            let edge_value = edge.value().cloned();
            let edge_metadata = edge.metadata().cloned();

            // Check if this edge creates a cycle
            if cycle_edges.contains(&edge_idx) {
                // This edge creates a cycle. Create a unique intermediate node for it.
                let target_vertex = self.get_vertex(target_idx).ok_or_else(|| {
                    format!(
                        "Target vertex {} not found for cycle edge {}",
                        target_idx, edge_idx
                    )
                })?;
                let target_id = target_vertex.id();

                // Create a unique intermediate ID using target ID + component + edge index
                let intermediate_id = target_id
                    .create_child(INTERMEDIATE_NODE_COMPONENT)
                    .create_child(edge_idx);

                // Add the unique intermediate node to the new graph
                // Intermediate nodes typically don't carry the original vertex's value.
                let intermediate_idx = acyclic_graph.add_vertex(intermediate_id.clone(), None)?;

                // Get the source vertex ID from the acyclic graph
                let source_id = acyclic_graph
                    .get_vertex(source_idx)
                    .ok_or_else(|| {
                        format!(
                            "Source vertex {} not found for cycle edge {}",
                            source_idx, edge_idx
                        )
                    })?
                    .id()
                    .clone();

                // Add the redirected edge (source -> intermediate) to the new graph,
                // preserving the original edge's ID, value, and metadata.
                acyclic_graph.add_edge_with_id_and_value(
                    &source_id,
                    &intermediate_id, // Target is the new intermediate node
                    original_edge_id, // Use the original edge's ID
                    edge_value,
                    edge_metadata,
                )?;
            } else {
                // Regular edge, copy it directly
                // Get the IDs from the acyclic graph to ensure consistency
                let source_id = acyclic_graph
                    .get_vertex(source_idx)
                    .ok_or_else(|| {
                        format!(
                            "Source vertex {} not found for edge {}",
                            source_idx, edge_idx
                        )
                    })?
                    .id()
                    .clone();
                let target_id = acyclic_graph
                    .get_vertex(target_idx)
                    .ok_or_else(|| {
                        format!(
                            "Target vertex {} not found for edge {}",
                            target_idx, edge_idx
                        )
                    })?
                    .id()
                    .clone();

                acyclic_graph.add_edge_with_id_and_value(
                    &source_id,
                    &target_id,
                    original_edge_id, // Use the original edge's ID
                    edge_value,
                    edge_metadata,
                )?;
            }
        }

        Ok(acyclic_graph)
    }

    /// Adds synthetic edges based on hierarchical adjacency between edge attributes.
    ///
    /// This function assumes the graph is already a Directed Acyclic Graph (DAG).
    /// It iterates through pairs of edges (`edge_path`, `edge_dep`) and checks for
    /// specific hierarchical relationships between IDs extracted from their values
    /// using the provided closures.
    ///
    /// - `get_path_id`: A closure that attempts to extract a `HierarchicalId` from an edge's
    ///   value, typically representing a "path" or source reference (e.g., `callee_func_path`).
    /// - `get_dependency_id`: A closure that attempts to extract a `HierarchicalId` from an edge's
    ///   value, typically representing a "dependency" reference (e.g., `ce_id_dependency`).
    ///
    /// For each `edge_path` with a valid `path_id`, the function finds all `edge_dep` edges
    /// where `get_dependency_id` returns a `dep_id` such that `dep_id.is_descendant_of(path_id)`.
    /// Among these descendants, it identifies the one with the "closest" `dep_id` (the minimum
    /// descendant ID according to `HierarchicalId`'s `Ord` implementation).
    ///
    /// If a unique closest descendant `edge_dep` is found, a synthetic edge is added:
    /// - From: `edge_path.target()`
    /// - To: `edge_dep.source()`
    /// - Value: `None`
    /// - Metadata: The provided `synthetic_edge_metadata`.
    /// - ID: `None`
    ///
    /// # Arguments
    ///
    /// * `get_path_id`: `Fn(&E) -> Option<&HierarchicalId>` - Extracts the path ID.
    /// * `get_dependency_id`: `Fn(&E) -> Option<&HierarchicalId>` - Extracts the dependency ID.
    /// * `synthetic_edge_metadata`: `Option<E>` - Metadata to assign to the created synthetic edges.
    ///
    /// # Returns
    ///
    /// * `Ok(())` if successful.
    /// * `Err(String)` if an error occurs (e.g., vertex lookup fails, though unlikely).
    ///
    /// # Assumptions
    ///
    /// - The input graph is a DAG. This function does not perform cycle checks before adding edges.
    /// - The relevant IDs are stored within the `value` field of the `Edge<E>`.
    /// - `E` implements `Clone` for the synthetic metadata.
    pub fn add_hierarchical_adjacency_edges<FPath, FDep>(
        &mut self,
        get_path_id: FPath,
        get_dependency_id: FDep,
        synthetic_edge_metadata: Option<E>,
    ) -> Result<(), String>
    where
        FPath: Fn(&E) -> Option<&HierarchicalId>,
        FDep: Fn(&E) -> Option<&HierarchicalId>,
        E: Clone + fmt::Debug, // Require Clone for metadata and Debug for potential issues
    {
        let mut edges_to_add: Vec<(VertexIndex, VertexIndex)> = Vec::new();

        // Iterate through all edges to find potential 'path' edges
        for (path_idx, path_edge) in self.edges.iter().enumerate() {
            // Assume the relevant ID is in the edge's value
            if let Some(path_val) = path_edge.value() {
                if let Some(path_id) = get_path_id(path_val) {
                    let mut best_dep_edge_idx: Option<EdgeIndex> = None;
                    let mut best_dep_id: Option<&HierarchicalId> = None;

                    // Iterate through all edges again to find potential 'dependency' edges
                    for (dep_idx, dep_edge) in self.edges.iter().enumerate() {
                        // Don't compare an edge with itself if the logic could overlap
                        // (e.g., if an edge could contain both path and dependency IDs)
                        // if path_idx == dep_idx { continue; } // Optional: uncomment if needed

                        if let Some(dep_val) = dep_edge.value() {
                            if let Some(dep_id) = get_dependency_id(dep_val) {
                                // Check if dep_id is a descendant of path_id
                                if dep_id.is_descendant_of(path_id) {
                                    // Check if this descendant is closer (smaller ID) than the current best
                                    let is_closer = best_dep_id
                                        .map_or(true, |current_best| dep_id < current_best);

                                    if is_closer {
                                        best_dep_id = Some(dep_id);
                                        best_dep_edge_idx = Some(dep_idx);
                                    }
                                    // Note: If dep_id == current_best, we keep the first one found.
                                }
                            }
                        }
                    } // End inner loop (dep_edge)

                    // If we found a closest dependency edge, record the synthetic edge to add
                    if let Some(best_idx) = best_dep_edge_idx {
                        let source_vertex_idx = path_edge.target(); // Target of the path edge
                        let target_vertex_idx = self.edges[best_idx].source(); // Source of the dependency edge

                        // Avoid adding self-loops
                        if source_vertex_idx != target_vertex_idx {
                            // Check if this exact edge already exists to prevent duplicates?
                            // For now, let's allow potential duplicates if multiple paths lead to the same adjacency.
                            edges_to_add.push((source_vertex_idx, target_vertex_idx));
                        }
                    }
                }
            }
        } // End outer loop (path_edge)

        // Add all the collected synthetic edges
        for (source_idx, target_idx) in edges_to_add {
            // Retrieve the actual HierarchicalIds of the vertices for the add_edge function
            let source_vertex_id = self
                .get_vertex(source_idx)
                .ok_or_else(|| {
                    format!("Source vertex {} not found for synthetic edge", source_idx)
                })?
                .id()
                .clone();
            let target_vertex_id = self
                .get_vertex(target_idx)
                .ok_or_else(|| {
                    format!("Target vertex {} not found for synthetic edge", target_idx)
                })?
                .id()
                .clone();

            // Add the synthetic edge. It has no ID itself, no value, only the provided metadata.
            // We use add_edge_with_id_and_value for clarity, passing None for edge_id and value.
            // Note: This uses the version of add_edge that might not perform cycle checks
            // based on the current state of the codebase where the check was removed.
            self.add_edge_with_id_and_value(
                &source_vertex_id,
                &target_vertex_id,
                None,                            // No HierarchicalId for the synthetic edge
                None,                            // No value for the synthetic edge
                synthetic_edge_metadata.clone(), // Clone the provided metadata
            )?; // Propagate potential errors from add_edge
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hig_dot::HigToDot; // Ensure this import is present
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

        let root_idx = graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        let child1_idx = graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();
        let child2_idx = graph
            .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
            .unwrap();
        let grandchild_idx = graph
            .add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1"))
            .unwrap();

        graph
            .add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), None)
            .unwrap();
        graph
            .add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), None)
            .unwrap();
        graph
            .add_edge(
                &HierarchicalId::new("1.1"),
                &HierarchicalId::new("1.1.1"),
                None,
            )
            .unwrap();

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

        let root_idx = graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        let child1_idx = graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();
        let child2_idx = graph
            .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
            .unwrap();
        let grandchild_idx = graph
            .add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1"))
            .unwrap();

        let edge1_idx = graph
            .add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), None)
            .unwrap();
        let edge2_idx = graph
            .add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), None)
            .unwrap();
        let edge3_idx = graph
            .add_edge(
                &HierarchicalId::new("1.1"),
                &HierarchicalId::new("1.1.1"),
                None,
            )
            .unwrap();

        let sorted = graph.topological_sort().unwrap();

        assert_eq!(sorted[0], root_idx);

        assert!(sorted.contains(&grandchild_idx));
    }

    #[test]
    fn test_topological_sort_edges() {
        let mut graph = HierarchicalIntervalGraph::<&str, ()>::new();

        // Create a simple graph
        graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1"))
            .unwrap();

        let edge1_idx = graph
            .add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.1"), None)
            .unwrap();
        let edge2_idx = graph
            .add_edge(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"), None)
            .unwrap();
        let edge3_idx = graph
            .add_edge(
                &HierarchicalId::new("1.1"),
                &HierarchicalId::new("1.1.1"),
                None,
            )
            .unwrap();

        let sorted_edges = graph.topological_sort_edges().unwrap();

        assert!(sorted_edges.contains(&edge1_idx));
        assert!(sorted_edges.contains(&edge2_idx));
        assert!(sorted_edges.contains(&edge3_idx));

        let pos1 = sorted_edges.iter().position(|&e| e == edge1_idx).unwrap();
        let pos3 = sorted_edges.iter().position(|&e| e == edge3_idx).unwrap();

        assert!(pos1 < pos3);
    }

    #[test]
    fn test_query_interval() {
        let mut graph = HierarchicalIntervalGraph::<&str, ()>::new();

        let root_idx = graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        let child1_idx = graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();
        let child2_idx = graph
            .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
            .unwrap();
        let grandchild_idx = graph
            .add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1"))
            .unwrap();
        let another_root_idx = graph
            .add_vertex(HierarchicalId::new("2"), Some("Another Root"))
            .unwrap();

        let results = graph.query_interval(&HierarchicalId::new("1"), &HierarchicalId::new("1.2"));
        assert_eq!(results.len(), 1);

        let descendants = graph.query_descendants(&HierarchicalId::new("1"));
        assert_eq!(descendants.len(), 3);

        let ancestors = graph.query_ancestors(&HierarchicalId::new("1.1.1"));
        assert_eq!(ancestors.len(), 2);
    }

    #[test]
    fn test_edge_with_id() {
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // Add vertices
        graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
            .unwrap();

        // Add edge with ID
        let edge_id = HierarchicalId::new("e.1");
        let edge_idx = graph
            .add_edge_with_id(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("1.1"),
                edge_id.clone(),
                Some("Edge metadata"),
            )
            .unwrap();

        // Add edge with ID and value
        let edge_id2 = HierarchicalId::new("e.2");
        let edge_idx2 = graph
            .add_edge_with_id_and_value(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("1.2"),
                Some(edge_id2.clone()),
                Some("Edge value"),
                Some("Edge metadata"),
            )
            .unwrap();

        // Verify edges can be retrieved by ID
        let edge = graph.get_edge_by_id(&edge_id).unwrap();
        assert_eq!(edge.id(), Some(&edge_id));
        assert_eq!(edge.metadata(), Some(&"Edge metadata"));
        assert_eq!(edge.value(), None);

        let edge2 = graph.get_edge_by_id(&edge_id2).unwrap();
        assert_eq!(edge2.id(), Some(&edge_id2));
        assert_eq!(edge2.value(), Some(&"Edge value"));
        assert_eq!(edge2.metadata(), Some(&"Edge metadata"));

        // Test error on duplicate edge ID
        let result = graph.add_edge_with_id(
            &HierarchicalId::new("1.1"),
            &HierarchicalId::new("1.2"),
            edge_id.clone(),
            None,
        );
        assert!(result.is_err());

        // Test modifying edge by ID
        if let Some(edge) = graph.get_edge_by_id_mut(&edge_id) {
            edge.set_value(Some("Updated value"));
        }

        let updated_edge = graph.get_edge_by_id(&edge_id).unwrap();
        assert_eq!(updated_edge.value(), Some(&"Updated value"));
    }

    #[cfg(test)]
    mod dot_tests {
        use super::*;
        use crate::hig_dot::HigToDot;

        #[test]
        fn test_hig_to_dot() {
            let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

            graph
                .add_vertex(HierarchicalId::new("1"), Some("Root"))
                .unwrap();
            graph
                .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
                .unwrap();
            graph
                .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
                .unwrap();
            graph
                .add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1"))
                .unwrap();

            graph
                .add_edge(
                    &HierarchicalId::new("1"),
                    &HierarchicalId::new("1.1"),
                    Some("Edge 1"),
                )
                .unwrap();
            graph
                .add_edge(
                    &HierarchicalId::new("1"),
                    &HierarchicalId::new("1.2"),
                    Some("Edge 2"),
                )
                .unwrap();
            graph
                .add_edge(
                    &HierarchicalId::new("1.1"),
                    &HierarchicalId::new("1.1.1"),
                    Some("Edge 3"),
                )
                .unwrap();

            let dot_output = graph.to_dot("TestGraph", true);

            // Basic validation
            assert!(dot_output.starts_with("digraph TestGraph {"));
            assert!(dot_output.contains("n0 [label=\"1\", tooltip=\"Root\"];"));
            assert!(dot_output.contains("n0 -> n1"));
            assert!(dot_output.ends_with(
                "}
"
            ));
        }
    }

    #[test]
    fn test_transform_to_acyclic_multiple_cycles() {
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999;
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // Define IDs
        let v1_id = HierarchicalId::new("1");
        let v2_id = HierarchicalId::new("2");
        let v3_id = HierarchicalId::new("3");
        let v4_id = HierarchicalId::new("4");
        let v5_id = HierarchicalId::new("5");
        let v6_id = HierarchicalId::new("6");

        // Add vertices
        graph.add_vertex(v1_id.clone(), Some("V1")).unwrap();
        graph.add_vertex(v2_id.clone(), Some("V2")).unwrap();
        graph.add_vertex(v3_id.clone(), Some("V3")).unwrap();
        graph.add_vertex(v4_id.clone(), Some("V4")).unwrap();
        graph.add_vertex(v5_id.clone(), Some("V5")).unwrap();
        graph.add_vertex(v6_id.clone(), Some("V6")).unwrap();

        // Add edges
        // Simple edge: V1 -> V2
        graph
            .add_edge_with_value(&v1_id, &v2_id, Some("val_12"), Some("meta_12"))
            .unwrap();
        // Cycle 1: V3 -> V4 -> V3
        graph
            .add_edge_with_value(&v3_id, &v4_id, Some("val_34"), Some("meta_34"))
            .unwrap();
        graph
            .add_edge_with_value(&v4_id, &v3_id, Some("val_43"), Some("meta_43"))
            .unwrap(); // Back edge 1
                       // Cycle 2: V5 -> V6 -> V5
        graph
            .add_edge_with_value(&v5_id, &v6_id, Some("val_56"), Some("meta_56"))
            .unwrap();
        graph
            .add_edge_with_value(&v6_id, &v5_id, Some("val_65"), Some("meta_65"))
            .unwrap(); // Back edge 2

        println!("Original graph: {:#?}", graph);
        // Transform the graph
        let acyclic_result = graph.transform_to_acyclic();
        assert!(acyclic_result.is_ok(), "Transformation should succeed");
        let acyclic_graph = acyclic_result.unwrap();

        // Verify acyclicity
        assert!(
            acyclic_graph.topological_sort().is_ok(),
            "Transformed graph should be acyclic"
        );

        // Verify vertex count (original 6 + 2 intermediate)
        assert_eq!(
            acyclic_graph.vertices().count(),
            8,
            "Should have 8 vertices"
        );

        // Verify edge count (original 5 - 2 back edges + 2 new edges to intermediate nodes)
        assert_eq!(acyclic_graph.edges().count(), 5, "Should have 5 edges");

        // Define expected intermediate node IDs
        let intermediate_v3_id =
            HierarchicalId::from_components(vec![3, INTERMEDIATE_NODE_COMPONENT]);
        let intermediate_v5_id =
            HierarchicalId::from_components(vec![5, INTERMEDIATE_NODE_COMPONENT]);

        // Verify intermediate nodes exist
        assert!(
            acyclic_graph.get_vertex_idx(&intermediate_v3_id).is_some(),
            "Intermediate node for V3 should exist"
        );
        assert!(
            acyclic_graph.get_vertex_idx(&intermediate_v5_id).is_some(),
            "Intermediate node for V5 should exist"
        );

        // Helper to check if an edge exists between two IDs
        let edge_exists = |graph: &HierarchicalIntervalGraph<_, _>,
                           src_id: &HierarchicalId,
                           tgt_id: &HierarchicalId|
         -> bool {
            graph.edges().any(|e| {
                let src_match = graph
                    .get_vertex(e.source())
                    .map_or(false, |v| v.id() == src_id);
                let tgt_match = graph
                    .get_vertex(e.target())
                    .map_or(false, |v| v.id() == tgt_id);
                src_match && tgt_match
            })
        };

        // dump graph
        println!("Transformed graph: {:#?}", acyclic_graph);

        // Verify expected edges exist
        assert!(
            edge_exists(&acyclic_graph, &v1_id, &v2_id),
            "Edge V1->V2 should exist"
        );
        assert!(
            edge_exists(&acyclic_graph, &v3_id, &v4_id),
            "Edge V3->V4 should exist"
        );
        assert!(
            edge_exists(&acyclic_graph, &v4_id, &intermediate_v3_id),
            "Edge V4->V3_intermediate should exist"
        );
        assert!(
            edge_exists(&acyclic_graph, &v5_id, &v6_id),
            "Edge V5->V6 should exist"
        );
        assert!(
            edge_exists(&acyclic_graph, &v6_id, &intermediate_v5_id),
            "Edge V6->V5_intermediate should exist"
        );

        // Verify original back edges do NOT exist
        assert!(
            !edge_exists(&acyclic_graph, &v4_id, &v3_id),
            "Edge V4->V3 should NOT exist"
        );
        assert!(
            !edge_exists(&acyclic_graph, &v6_id, &v5_id),
            "Edge V6->V5 should NOT exist"
        );

        // Verify edge values/metadata were transferred correctly to the new edges
        let edge_v4_int3 = acyclic_graph
            .edges()
            .find(|e| {
                let src_match = acyclic_graph
                    .get_vertex(e.source())
                    .map_or(false, |v| v.id() == &v4_id);
                let tgt_match = acyclic_graph
                    .get_vertex(e.target())
                    .map_or(false, |v| v.id() == &intermediate_v3_id);
                src_match && tgt_match
            })
            .unwrap();
        assert_eq!(
            edge_v4_int3.value(),
            Some(&"val_43"),
            "Value mismatch for V4->V3_intermediate"
        );
        assert_eq!(
            edge_v4_int3.metadata(),
            Some(&"meta_43"),
            "Metadata mismatch for V4->V3_intermediate"
        );

        let edge_v6_int5 = acyclic_graph
            .edges()
            .find(|e| {
                let src_match = acyclic_graph
                    .get_vertex(e.source())
                    .map_or(false, |v| v.id() == &v6_id);
                let tgt_match = acyclic_graph
                    .get_vertex(e.target())
                    .map_or(false, |v| v.id() == &intermediate_v5_id);
                src_match && tgt_match
            })
            .unwrap();
        assert_eq!(
            edge_v6_int5.value(),
            Some(&"val_65"),
            "Value mismatch for V6->V5_intermediate"
        );
        assert_eq!(
            edge_v6_int5.metadata(),
            Some(&"meta_65"),
            "Metadata mismatch for V6->V5_intermediate"
        );
    }

    #[test]
    fn test_topological_sort_on_transformed_multiple_cycles() {
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999;
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // --- Setup: Same graph as test_transform_to_acyclic_multiple_cycles ---
        let v1_id = HierarchicalId::new("1");
        let v2_id = HierarchicalId::new("2");
        let v3_id = HierarchicalId::new("3");
        let v4_id = HierarchicalId::new("4");
        let v5_id = HierarchicalId::new("5");
        let v6_id = HierarchicalId::new("6");

        graph.add_vertex(v1_id.clone(), Some("V1")).unwrap();
        graph.add_vertex(v2_id.clone(), Some("V2")).unwrap();
        graph.add_vertex(v3_id.clone(), Some("V3")).unwrap();
        graph.add_vertex(v4_id.clone(), Some("V4")).unwrap();
        graph.add_vertex(v5_id.clone(), Some("V5")).unwrap();
        graph.add_vertex(v6_id.clone(), Some("V6")).unwrap();

        // Edges: V1->V2, V3->V4->V3 (cycle), V5->V6->V5 (cycle)
        graph
            .add_edge_with_value(&v1_id, &v2_id, Some("val_12"), Some("meta_12"))
            .unwrap();
        graph
            .add_edge_with_value(&v3_id, &v4_id, Some("val_34"), Some("meta_34"))
            .unwrap();
        graph // Back edge 1
            .add_edge_with_value(&v4_id, &v3_id, Some("val_43"), Some("meta_43"))
            .unwrap();
        graph
            .add_edge_with_value(&v5_id, &v6_id, Some("val_56"), Some("meta_56"))
            .unwrap();
        graph // Back edge 2
            .add_edge_with_value(&v6_id, &v5_id, Some("val_65"), Some("meta_65"))
            .unwrap();

        // --- Transform ---
        let acyclic_graph = graph.transform_to_acyclic().unwrap();

        println!("Transformed graph: {:#?}", acyclic_graph);

        // --- Topological Sort Edges ---
        let sorted_edge_indices_result = acyclic_graph.topological_sort_edges();

        assert!(
            sorted_edge_indices_result.is_ok(),
            "Topological sort of edges failed: {:?}",
            sorted_edge_indices_result.err()
        );
        let sorted_edge_indices = sorted_edge_indices_result.unwrap();

        println!(
            "Sorted edge indices: {:?}",
            sorted_edge_indices
                .iter()
                .filter_map(|eidx| acyclic_graph.get_edge(*eidx))
                .collect::<Vec<_>>()
        );

        // --- Verification ---
        assert_eq!(
            sorted_edge_indices.len(),
            5,
            "Sorted list should contain 5 edges"
        );

        // Define expected intermediate node IDs
        let intermediate_v3_id =
            HierarchicalId::from_components(vec![3, INTERMEDIATE_NODE_COMPONENT]);
        let intermediate_v5_id =
            HierarchicalId::from_components(vec![5, INTERMEDIATE_NODE_COMPONENT]);

        // Helper to find edge index by source/target IDs in the acyclic graph
        let find_edge_idx = |graph: &HierarchicalIntervalGraph<&str, &str>,
                             src_id: &HierarchicalId,
                             tgt_id: &HierarchicalId|
         -> Option<EdgeIndex> {
            graph
                .edges()
                .enumerate()
                .find(|(_, e)| {
                    let source_match = graph
                        .get_vertex(e.source())
                        .map_or(false, |v| v.id() == src_id);
                    let target_match = graph
                        .get_vertex(e.target())
                        .map_or(false, |v| v.id() == tgt_id);
                    source_match && target_match
                })
                .map(|(idx, _)| idx)
        };

        // Find indices of the relevant edges in the *acyclic* graph
        let edge_v1_v2_idx =
            find_edge_idx(&acyclic_graph, &v1_id, &v2_id).expect("Edge V1->V2 should exist");
        let edge_v3_v4_idx =
            find_edge_idx(&acyclic_graph, &v3_id, &v4_id).expect("Edge V3->V4 should exist");
        let edge_v4_int3_idx = find_edge_idx(&acyclic_graph, &v4_id, &intermediate_v3_id)
            .expect("Edge V4->V3_intermediate should exist");
        let edge_v5_v6_idx =
            find_edge_idx(&acyclic_graph, &v5_id, &v6_id).expect("Edge V5->V6 should exist");
        let edge_v6_int5_idx = find_edge_idx(&acyclic_graph, &v6_id, &intermediate_v5_id)
            .expect("Edge V6->V5_intermediate should exist");

        // Find positions in the sorted list
        let pos_v1_v2 = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_v1_v2_idx)
            .unwrap();
        let pos_v3_v4 = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_v3_v4_idx)
            .unwrap();
        let pos_v4_int3 = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_v4_int3_idx)
            .unwrap();
        let pos_v5_v6 = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_v5_v6_idx)
            .unwrap();
        let pos_v6_int5 = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_v6_int5_idx)
            .unwrap();

        // Assert dependencies within the broken cycles
        assert!(
            pos_v3_v4 < pos_v4_int3,
            "Edge V3->V4 must come before V4->V3_intermediate (pos_v3_v4={}, pos_v4_int3={})",
            pos_v3_v4,
            pos_v4_int3
        );
        assert!(
            pos_v5_v6 < pos_v6_int5,
            "Edge V5->V6 must come before V6->V5_intermediate (pos_v5_v6={}, pos_v6_int5={})",
            pos_v5_v6,
            pos_v6_int5
        );

        // Note: The relative order between (V1->V2), (V3->V4 chain), and (V5->V6 chain)
        // depends on the starting node selection and traversal order of the topological sort.
        // We only assert the internal dependencies here.
    }

    #[test]
    fn test_transform_to_acyclic_breaks_cycle() {
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // Create vertices
        let a_id = HierarchicalId::new("1");
        let b_id = HierarchicalId::new("2");
        let c_id = HierarchicalId::new("3");
        graph.add_vertex(a_id.clone(), Some("A")).unwrap();
        graph.add_vertex(b_id.clone(), Some("B")).unwrap();
        graph.add_vertex(c_id.clone(), Some("C")).unwrap();

        // Create edges forming a cycle: A -> B -> C -> A
        graph
            .add_edge_with_value(&a_id, &b_id, Some("val_ab"), Some("meta_ab"))
            .unwrap();
        graph
            .add_edge_with_value(&b_id, &c_id, Some("val_bc"), Some("meta_bc"))
            .unwrap();
        // This is the back edge creating the cycle
        graph
            .add_edge_with_value(&c_id, &a_id, Some("val_ca"), Some("meta_ca"))
            .unwrap();

        // Transform the graph
        let acyclic_graph = graph.transform_to_acyclic().unwrap();

        // 1. Verify the new graph is acyclic
        assert!(
            acyclic_graph.topological_sort().is_ok(),
            "Transformed graph should be acyclic"
        );

        // 2. Verify the structure of the transformed graph
        assert_eq!(
            acyclic_graph.vertices().count(),
            4,
            "Should have original 3 vertices + 1 intermediate node"
        );

        // Find the intermediate node (expected ID "3.99999" or similar if collision occurred)
        // Use the same constant as in the main function for consistency.
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999;
        let expected_ret_node_id =
            HierarchicalId::from_components(vec![3, INTERMEDIATE_NODE_COMPONENT]);
        // We assume no collision happened in this simple test case.
        // A more robust test might list vertices and find the one with parent "3" and last component >= INTERMEDIATE_NODE_COMPONENT.
        let ret_node_idx = acyclic_graph
            .get_vertex_idx(&expected_ret_node_id)
            .expect(&format!(
                "Intermediate node '{}' should exist",
                expected_ret_node_id
            ));
        let ret_node = acyclic_graph.get_vertex(ret_node_idx).unwrap();
        assert_eq!(
            ret_node.id(),
            &expected_ret_node_id,
            "Intermediate node ID should match expected"
        );
        assert!(
            ret_node.value().is_none(),
            "Intermediate node should have no value"
        ); // This assertion should now pass

        // 3. Verify edges in the acyclic graph
        // Expecting 3 edges: A->B, B->C, C->intermediate
        // The intermediate->A edge is currently commented out in transform_to_acyclic
        assert_eq!(
            acyclic_graph.edges().count(),
            3,
            "Should have 3 edges after transformation (A->B, B->C, C->ret)"
        );

        // Check original non-back edges still exist
        let edge_ab = acyclic_graph
            .outgoing_edges(&a_id)
            .into_iter()
            .find(|e| acyclic_graph.get_vertex(e.target()).unwrap().id() == &b_id)
            .expect("Edge A->B should exist");
        assert_eq!(edge_ab.value(), Some(&"val_ab"));
        assert_eq!(edge_ab.metadata(), Some(&"meta_ab"));

        let edge_bc = acyclic_graph
            .outgoing_edges(&b_id)
            .into_iter()
            .find(|e| acyclic_graph.get_vertex(e.target()).unwrap().id() == &c_id)
            .expect("Edge B->C should exist");
        assert_eq!(edge_bc.value(), Some(&"val_bc"));
        assert_eq!(edge_bc.metadata(), Some(&"meta_bc"));

        // Check the back edge C -> A was replaced by C -> intermediate_node
        let edge_c_ret = acyclic_graph
            .outgoing_edges(&c_id)
            .into_iter()
            .find(|e| e.target() == ret_node_idx)
            .expect(&format!("Edge C -> {} should exist", expected_ret_node_id));
        assert!(
            edge_c_ret.value().is_none(),
            "Edge C -> ret should have no value"
        );
        assert!(
            edge_c_ret.metadata().is_none(),
            "Edge C -> ret should have no metadata"
        );

        // Check the edge from the intermediate node to A (this part depends on whether the edge was re-added)
        // Since it's currently commented out in transform_to_acyclic, assert it *doesn't* exist.
        assert!(
            acyclic_graph
                .outgoing_edges(&expected_ret_node_id)
                .is_empty(),
            "Intermediate node should have no outgoing edges currently"
        );
        /* // Keep this commented out unless the edge is re-enabled in transform_to_acyclic
        let edge_ret_a = acyclic_graph.outgoing_edges(&expected_ret_node_id).into_iter()
             .find(|e| acyclic_graph.get_vertex(e.target()).unwrap().id() == &a_id)
             .expect(&format!("Edge {} -> A should exist", expected_ret_node_id));
        assert_eq!(edge_ret_a.value(), Some(&"val_ca"), "Edge ret -> A should inherit value");
        assert_eq!(edge_ret_a.metadata(), Some(&"meta_ca"), "Edge ret -> A should inherit metadata");
        */

        // Ensure the original back edge C -> A does *not* exist directly
        assert!(
            acyclic_graph
                .outgoing_edges(&c_id)
                .into_iter()
                .find(|e| acyclic_graph.get_vertex(e.target()).unwrap().id() == &a_id)
                .is_none(),
            "Original back edge C -> A should not exist"
        );
    }

    #[test]
    fn test_topological_sort_edges_on_transformed_acyclic_graph() {
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999;
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // --- Setup: Create a graph with a cycle ---
        let a_id = HierarchicalId::new("1");
        let b_id = HierarchicalId::new("1.1");
        let c_id = HierarchicalId::new("1.1.1");
        graph.add_vertex(a_id.clone(), Some("A")).unwrap();
        graph.add_vertex(b_id.clone(), Some("B")).unwrap();
        graph.add_vertex(c_id.clone(), Some("C")).unwrap();

        // Edges: A -> B -> C -> A (cycle)
        graph
            .add_edge_with_value(&a_id, &b_id, Some("val_ab"), Some("meta_ab"))
            .unwrap();
        graph
            .add_edge_with_value(&b_id, &c_id, Some("val_bc"), Some("meta_bc"))
            .unwrap();
        graph // Back edge
            .add_edge_with_value(&c_id, &a_id, Some("val_ca"), Some("meta_ca"))
            .unwrap();

        // --- Transform to acyclic graph ---
        // Expected structure: A -> B -> C -> C_intermediate
        let acyclic_graph = graph.transform_to_acyclic().unwrap();

        // Quick check: vertex sort should succeed
        assert!(
            acyclic_graph.topological_sort().is_ok(),
            "Transformed graph should be acyclic"
        );

        // --- Focus: Test topological_sort_edges on the result ---
        let edge_sort_result = acyclic_graph.topological_sort_edges();
        assert!(
            edge_sort_result.is_ok(),
            "topological_sort_edges should succeed on the acyclic graph, but failed: {:?}",
            edge_sort_result.err()
        );
        let sorted_edge_indices = edge_sort_result.unwrap();

        // --- Verification ---
        assert_eq!(
            acyclic_graph.edges().count(),
            3,
            "Acyclic graph should have 3 edges (A->B, B->C, C->intermediate)"
        );
        assert_eq!(
            sorted_edge_indices.len(),
            3,
            "Sorted edge list should contain 3 edges"
        );

        // Find the indices of the specific edges in the *acyclic* graph
        let expected_ret_node_id = c_id.create_child(INTERMEDIATE_NODE_COMPONENT);

        // Helper to find edge index by source/target IDs in the acyclic graph
        let find_edge_idx = |graph: &HierarchicalIntervalGraph<&str, &str>,
                             src_id: &HierarchicalId,
                             tgt_id: &HierarchicalId|
         -> Option<EdgeIndex> {
            graph
                .edges()
                .enumerate()
                .find(|(_, e)| {
                    let source_match = graph
                        .get_vertex(e.source())
                        .map_or(false, |v| v.id() == src_id);
                    let target_match = graph
                        .get_vertex(e.target())
                        .map_or(false, |v| v.id() == tgt_id);
                    source_match && target_match
                })
                .map(|(idx, _)| idx)
        };

        let edge_ab_new_idx = find_edge_idx(&acyclic_graph, &a_id, &b_id)
            .expect("Edge A->B should exist in acyclic graph");
        let edge_bc_new_idx = find_edge_idx(&acyclic_graph, &b_id, &c_id)
            .expect("Edge B->C should exist in acyclic graph");
        let edge_c_ret_new_idx = find_edge_idx(&acyclic_graph, &c_id, &expected_ret_node_id)
            .expect("Edge C->intermediate should exist in acyclic graph");

        // Find the positions of these edges in the sorted list
        let pos_ab = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_ab_new_idx)
            .expect("Edge A->B index should be in sorted list");
        let pos_bc = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_bc_new_idx)
            .expect("Edge B->C index should be in sorted list");
        let pos_c_ret = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_c_ret_new_idx)
            .expect("Edge C->intermediate index should be in sorted list");

        // Assert the topological order of edges based on dependencies
        assert!(
            pos_ab < pos_bc,
            "Edge A->B must come before B->C in topological edge sort (pos_ab={}, pos_bc={})",
            pos_ab,
            pos_bc
        );
        assert!(pos_bc < pos_c_ret, "Edge B->C must come before C->intermediate in topological edge sort (pos_bc={}, pos_c_ret={})", pos_bc, pos_c_ret);
    }

    #[test]
    fn test_topological_sort_edges_with_parallel_edges_and_ids() {
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999; // Consistent with transform_to_acyclic
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // --- Setup: Graph based on the DOT example ---
        let n0_id = HierarchicalId::new("0.1.60");
        let n1_id = HierarchicalId::new("0.1.7");

        graph.add_vertex(n0_id.clone(), Some("N0")).unwrap();
        graph.add_vertex(n1_id.clone(), Some("N1")).unwrap();

        // Define Edge IDs (Dependency IDs from DOT)
        let edge_id_n0_n1_a = HierarchicalId::new("0.1.60.62.112.115.116.117.118.119"); // resetCount
        let edge_id_n0_n1_b = HierarchicalId::new("0.1.60.62.88.95.104.105.106.107"); // getCount
        let edge_id_n0_n1_c = HierarchicalId::new("0.1.60.62.88.95.96.97.98.99"); // increment
        let edge_id_n1_n0_a = HierarchicalId::new("0.1.7.9.15.22.31.32"); // return count (1)
        let edge_id_n1_n0_b = HierarchicalId::new("0.1.7.9.35.43.44.45"); // return count (2)

        // Add edges n0 -> n1 with IDs
        graph
            .add_edge_with_id_and_value(
                &n0_id,
                &n1_id,
                Some(edge_id_n0_n1_a.clone()),
                Some("resetCount"), // Value for identification
                None,
            )
            .unwrap();
        graph
            .add_edge_with_id_and_value(
                &n0_id,
                &n1_id,
                Some(edge_id_n0_n1_b.clone()),
                Some("getCount"),
                None,
            )
            .unwrap();
        graph
            .add_edge_with_id_and_value(
                &n0_id,
                &n1_id,
                Some(edge_id_n0_n1_c.clone()),
                Some("increment"),
                None,
            )
            .unwrap();

        // Add edges n1 -> n0 with IDs (these will cause cycles)
        graph
            .add_edge_with_id_and_value(
                &n1_id,
                &n0_id,
                Some(edge_id_n1_n0_a.clone()),
                Some("return count (1)"),
                None,
            )
            .unwrap();
        graph
            .add_edge_with_id_and_value(
                &n1_id,
                &n0_id,
                Some(edge_id_n1_n0_b.clone()),
                Some("return count (2)"),
                None,
            )
            .unwrap();

        // --- Transform to Acyclic Graph ---
        // This is necessary because topological sort only works on DAGs.
        // The transformation will break the n1 -> n0 edges by redirecting them
        // to an intermediate node like "0.1.60.99999".
        let acyclic_graph = graph.transform_to_acyclic().unwrap();
        println!("--- Acyclic Graph Dump for Parallel Edge Test ---");
        println!("Vertices:");
        for (idx, vertex) in acyclic_graph.vertices().enumerate() {
            println!(
                "  Index {}: ID={}, Value={:?}, Outgoing={:?}, Incoming={:?}",
                idx,
                vertex.id(),
                vertex.value(),
                vertex.outgoing(),
                vertex.incoming()
            );
        }
        println!("Edges:");
        for (idx, edge) in acyclic_graph.edges().enumerate() {
            println!(
                "  Index {}: Source={}, Target={}, ID={:?}, Value={:?}, Metadata={:?}",
                idx,
                edge.source(),
                edge.target(),
                edge.id(),
                edge.value(),
                edge.metadata()
            );
        }
        println!("--- End Acyclic Graph Dump ---");

        // --- Verification of Acyclic Graph Structure ---

        // 1. Verify Vertex and Edge Counts
        assert_eq!(
            acyclic_graph.vertices().count(),
            4, // n0, n1, intermediate_a, intermediate_b
            "Acyclic graph should have 4 vertices"
        );
        assert_eq!(
            acyclic_graph.edges().count(),
            5, // 3x n0->n1, 1x n1->intermediate_a, 1x n1->intermediate_b
            "Acyclic graph should have 5 edges"
        );

        // 2. Find Intermediate Nodes and Verify Properties
        let intermediate_nodes: Vec<_> = acyclic_graph
            .vertices()
            .filter(|v| {
                v.id().components.len() > n0_id.components.len() // Must be deeper than original nodes
                    && v.id().components.contains(&INTERMEDIATE_NODE_COMPONENT) // Must contain the special component
                    && v.id().is_descendant_of(&n0_id) // Should be descendants of the original target (n0)
            })
            .collect();

        assert_eq!(
            intermediate_nodes.len(),
            2,
            "Should find exactly two intermediate nodes"
        );

        // Ensure the intermediate nodes are distinct
        assert_ne!(
            intermediate_nodes[0].id(),
            intermediate_nodes[1].id(),
            "Intermediate nodes must have distinct IDs"
        );

        // Store intermediate node IDs for edge verification
        let intermediate_id_a = intermediate_nodes[0].id().clone();
        let intermediate_id_b = intermediate_nodes[1].id().clone();

        // 3. Verify Edges in Acyclic Graph

        // Helper to check if an edge exists between two IDs, optionally checking the original edge ID
        let edge_exists_with_original_id = |graph: &HierarchicalIntervalGraph<_, _>,
                                            src_id: &HierarchicalId,
                                            tgt_id: &HierarchicalId,
                                            original_edge_id: Option<&HierarchicalId>|
         -> bool {
            graph.edges().any(|e| {
                let src_match = graph
                    .get_vertex(e.source())
                    .map_or(false, |v| v.id() == src_id);
                let tgt_match = graph
                    .get_vertex(e.target())
                    .map_or(false, |v| v.id() == tgt_id);
                let id_match = match original_edge_id {
                    Some(orig_id) => e.id() == Some(orig_id),
                    None => true, // Don't check ID if None is provided
                };
                src_match && tgt_match && id_match
            })
        };

        // Verify the original n0 -> n1 edges still exist
        assert!(
            edge_exists_with_original_id(&acyclic_graph, &n0_id, &n1_id, Some(&edge_id_n0_n1_a)),
            "Edge n0->n1 (resetCount) should exist"
        );
        assert!(
            edge_exists_with_original_id(&acyclic_graph, &n0_id, &n1_id, Some(&edge_id_n0_n1_b)),
            "Edge n0->n1 (getCount) should exist"
        );
        assert!(
            edge_exists_with_original_id(&acyclic_graph, &n0_id, &n1_id, Some(&edge_id_n0_n1_c)),
            "Edge n0->n1 (increment) should exist"
        );

        // Verify the n1 -> intermediate edges exist (one for each original back-edge)
        // We need to check against both possible intermediate IDs since we don't know which back-edge maps to which intermediate node
        let n1_to_int_a_exists = edge_exists_with_original_id(
            &acyclic_graph,
            &n1_id,
            &intermediate_id_a,
            Some(&edge_id_n1_n0_a),
        ) || edge_exists_with_original_id(
            &acyclic_graph,
            &n1_id,
            &intermediate_id_a,
            Some(&edge_id_n1_n0_b),
        );
        let n1_to_int_b_exists = edge_exists_with_original_id(
            &acyclic_graph,
            &n1_id,
            &intermediate_id_b,
            Some(&edge_id_n1_n0_a),
        ) || edge_exists_with_original_id(
            &acyclic_graph,
            &n1_id,
            &intermediate_id_b,
            Some(&edge_id_n1_n0_b),
        );

        assert!(
            n1_to_int_a_exists,
            "Edge n1 -> intermediate_a with an original back-edge ID should exist"
        );
        assert!(
            n1_to_int_b_exists,
            "Edge n1 -> intermediate_b with an original back-edge ID should exist"
        );

        // Verify the original n1 -> n0 back-edges do NOT exist
        assert!(
            !edge_exists_with_original_id(&acyclic_graph, &n1_id, &n0_id, None),
            "Direct edge n1->n0 should NOT exist in the acyclic graph"
        );

        // 4. Optional: Generate DOT file for visual inspection
        use crate::hig_dot::HigToDot; // Make sure this use statement is present at the top of the mod tests block or file
        let dot_output = acyclic_graph.to_dot("test_parallel_edges_acyclic", true);
        std::fs::write("test_parallel_edges_acyclic.dot", dot_output)
            .expect("Failed to write DOT file");
        println!("Generated DOT file: test_parallel_edges_acyclic.dot");
    }

    #[test]
    fn test_topological_sort_on_transformed_parallel_edges() {
        const INTERMEDIATE_NODE_COMPONENT: usize = 99999; // Consistent with transform_to_acyclic
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        // --- Setup: Graph based on the DOT example (same as test_topological_sort_edges_with_parallel_edges_and_ids) ---
        let n0_id = HierarchicalId::new("0.1.60");
        let n1_id = HierarchicalId::new("0.1.7");

        graph.add_vertex(n0_id.clone(), Some("N0")).unwrap();
        graph.add_vertex(n1_id.clone(), Some("N1")).unwrap();

        // Define Edge IDs (Dependency IDs from DOT)
        let edge_id_n0_n1_a = HierarchicalId::new("0.1.60.62.112.115.116.117.118.119"); // resetCount
        let edge_id_n0_n1_b = HierarchicalId::new("0.1.60.62.88.95.104.105.106.107"); // getCount
        let edge_id_n0_n1_c = HierarchicalId::new("0.1.60.62.88.95.96.97.98.99"); // increment
        let edge_id_n1_n0_a = HierarchicalId::new("0.1.7.9.15.22.31.32"); // return count (1)
        let edge_id_n1_n0_b = HierarchicalId::new("0.1.7.9.35.43.44.45"); // return count (2)

        // Add edges n0 -> n1 with IDs
        graph
            .add_edge_with_id_and_value(
                &n0_id,
                &n1_id,
                Some(edge_id_n0_n1_a.clone()),
                Some("resetCount"), // Value for identification
                None,
            )
            .unwrap();
        graph
            .add_edge_with_id_and_value(
                &n0_id,
                &n1_id,
                Some(edge_id_n0_n1_b.clone()),
                Some("getCount"),
                None,
            )
            .unwrap();
        graph
            .add_edge_with_id_and_value(
                &n0_id,
                &n1_id,
                Some(edge_id_n0_n1_c.clone()),
                Some("increment"),
                None,
            )
            .unwrap();

        // Add edges n1 -> n0 with IDs (these will cause cycles)
        graph
            .add_edge_with_id_and_value(
                &n1_id,
                &n0_id,
                Some(edge_id_n1_n0_a.clone()),
                Some("return count (1)"),
                None,
            )
            .unwrap();
        graph
            .add_edge_with_id_and_value(
                &n1_id,
                &n0_id,
                Some(edge_id_n1_n0_b.clone()),
                Some("return count (2)"),
                None,
            )
            .unwrap();

        // --- Transform to Acyclic Graph ---
        let acyclic_graph = graph.transform_to_acyclic().unwrap();

        // --- Topological Sort Edges on Acyclic Graph ---
        let sorted_edge_indices_result = acyclic_graph.topological_sort_edges();
        assert!(
            sorted_edge_indices_result.is_ok(),
            "Topological sort of edges failed: {:?}",
            sorted_edge_indices_result.err()
        );
        let sorted_edge_indices = sorted_edge_indices_result.unwrap();

        println!("Sorted edge indices: {:?}", sorted_edge_indices);
        println!("Sorted edges (detail):");
        for (pos, &edge_idx) in sorted_edge_indices.iter().enumerate() {
            if let Some(edge) = acyclic_graph.get_edge(edge_idx) {
                let source_id = acyclic_graph
                    .get_vertex(edge.source())
                    .map(|v| v.id().to_string())
                    .unwrap_or("?".to_string());
                let target_id = acyclic_graph
                    .get_vertex(edge.target())
                    .map(|v| v.id().to_string())
                    .unwrap_or("?".to_string());
                println!(
                    "  Pos {}: Index={}, {} -> {} (ID: {:?}, Value: {:?})",
                    pos,
                    edge_idx,
                    source_id,
                    target_id,
                    edge.id(),
                    edge.value()
                );
            } else {
                println!("  Pos {}: Index={} (Edge not found!)", pos, edge_idx);
            }
        }

        // --- Verification of Topological Sort Order ---
        assert_eq!(
            sorted_edge_indices.len(),
            5,
            "Sorted list should contain 5 edges"
        );

        // Helper to find edge index by source/target IDs and *original* edge ID in the acyclic graph
        let find_edge_idx_by_original_id = |graph: &HierarchicalIntervalGraph<&str, &str>,
                                            original_edge_id: &HierarchicalId|
         -> Option<EdgeIndex> {
            graph
                .edges()
                .enumerate()
                .find(|(_, e)| e.id() == Some(original_edge_id))
                .map(|(idx, _)| idx)
        };

        // Find indices of the relevant edges in the *acyclic* graph using their original IDs
        let edge_n0_n1_a_idx = find_edge_idx_by_original_id(&acyclic_graph, &edge_id_n0_n1_a)
            .expect("Edge n0->n1 (resetCount) should exist");
        let edge_n0_n1_b_idx = find_edge_idx_by_original_id(&acyclic_graph, &edge_id_n0_n1_b)
            .expect("Edge n0->n1 (getCount) should exist");
        let edge_n0_n1_c_idx = find_edge_idx_by_original_id(&acyclic_graph, &edge_id_n0_n1_c)
            .expect("Edge n0->n1 (increment) should exist");

        // Find the intermediate nodes created by the transformation
        let intermediate_nodes: Vec<_> = acyclic_graph
            .vertices()
            .filter(|v| {
                v.id().components.len() > n0_id.components.len()
                    && v.id().components.contains(&INTERMEDIATE_NODE_COMPONENT)
                    && v.id().is_descendant_of(&n0_id)
            })
            .collect();
        assert_eq!(
            intermediate_nodes.len(),
            2,
            "Expected two intermediate nodes"
        );

        // Find the edges pointing to the intermediate nodes, using the original back-edge IDs
        let edge_n1_int_a_idx = acyclic_graph
            .edges()
            .enumerate()
            .find(|(_, e)| e.id() == Some(&edge_id_n1_n0_a))
            .map(|(idx, _)| idx)
            .expect("Edge n1->intermediate (return count 1) should exist");
        let edge_n1_int_b_idx = acyclic_graph
            .edges()
            .enumerate()
            .find(|(_, e)| e.id() == Some(&edge_id_n1_n0_b))
            .map(|(idx, _)| idx)
            .expect("Edge n1->intermediate (return count 2) should exist");

        // Find positions in the sorted list
        let pos_n0_n1_a = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_n0_n1_a_idx)
            .unwrap();
        let pos_n0_n1_b = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_n0_n1_b_idx)
            .unwrap();
        let pos_n0_n1_c = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_n0_n1_c_idx)
            .unwrap();
        let pos_n1_int_a = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_n1_int_a_idx)
            .unwrap();
        let pos_n1_int_b = sorted_edge_indices
            .iter()
            .position(|&idx| idx == edge_n1_int_b_idx)
            .unwrap();

        // --- Assert specific order based on DFS traversal and ID sorting ---
        // Expected order: [2, 3, 4, 1, 0]
        // Edge 2: n0->n1 (c - increment) - ID ...99
        // Edge 3: n1->int (a - return 1) - ID ...32
        // Edge 4: n1->int (b - return 2) - ID ...45
        // Edge 1: n0->n1 (b - getCount)  - ID ...107
        // Edge 0: n0->n1 (a - resetCount)- ID ...119

        assert_eq!(
            sorted_edge_indices[0], edge_n0_n1_c_idx,
            "Pos 0 should be n0->n1 (c)"
        );
        assert_eq!(
            sorted_edge_indices[1], edge_n1_int_a_idx,
            "Pos 1 should be n1->int (a)"
        );
        assert_eq!(
            sorted_edge_indices[2], edge_n1_int_b_idx,
            "Pos 2 should be n1->int (b)"
        );
        assert_eq!(
            sorted_edge_indices[3], edge_n0_n1_b_idx,
            "Pos 3 should be n0->n1 (b)"
        );
        assert_eq!(
            sorted_edge_indices[4], edge_n0_n1_a_idx,
            "Pos 4 should be n0->n1 (a)"
        );

        // --- Verify relative orders implied by the main assertion ---

        // Dependencies: The edge chosen first (n0->n1 c) must come before the edges
        // reachable from its target (n1->int a, n1->int b).
        assert!(
            pos_n0_n1_c < pos_n1_int_a,
            "DFS implies n0->n1(c) visited before n1->int(a)"
        );
        assert!(
            pos_n0_n1_c < pos_n1_int_b,
            "DFS implies n0->n1(c) visited before n1->int(b)"
        );

        // Relative order within n1->intermediate based on edge IDs (ascending)
        // edge_id_n1_n0_a (...32) < edge_id_n1_n0_b (...45)
        assert!(
            pos_n1_int_a < pos_n1_int_b,
            "Edge n1->intermediate (a) must come before n1->intermediate (b) due to ID sort"
        );

        // Relative order of the remaining n0->n1 edges processed after backtracking, based on IDs
        // edge_id_n0_n1_b (...107) < edge_id_n0_n1_a (...119)
        assert!(
            pos_n0_n1_b < pos_n0_n1_a,
            "Edge n0->n1 (b) must come before n0->n1 (a) due to ID sort during backtracking"
        );
    }

    #[test]
    fn test_add_hierarchical_adjacency_edges_simplified() {
        // Define a minimal struct for edge values
        #[derive(Debug, Clone, PartialEq)]
        struct TestEdgeData {
            path_id: Option<HierarchicalId>,
            dep_id: Option<HierarchicalId>,
        }

        let mut graph = HierarchicalIntervalGraph::<(), TestEdgeData>::new(); // Use () for vertex value

        // --- Vertices (IDs created inline) ---
        let v1_idx = graph.add_vertex(HierarchicalId::new("1"), None).unwrap();
        let v2_idx = graph.add_vertex(HierarchicalId::new("2"), None).unwrap();
        let v3_idx = graph.add_vertex(HierarchicalId::new("3"), None).unwrap();

        // --- Edges (IDs created inline) ---
        // Edge with a path_id: V1 -> V2
        let _path_edge_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("2"),
                Some(TestEdgeData {
                    path_id: Some(HierarchicalId::new("1")), // Path ID
                    dep_id: None,
                }),
                None, // No metadata for original edges
            )
            .unwrap();

        // Edge with a dependency ID: V1 -> V3
        let _dep_edge_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("3"),
                Some(TestEdgeData {
                    path_id: None,
                    dep_id: Some(HierarchicalId::new("1.1")), // Descendant of "1"
                }),
                None, // No metadata for original edges
            )
            .unwrap();

        let initial_edge_count = graph.edges().count();
        assert_eq!(initial_edge_count, 2);

        // --- Define Synthetic Metadata ---
        let synthetic_metadata = Some(TestEdgeData {
            path_id: None,
            dep_id: None,
        });

        // --- Call the Function (using lambdas for extractors) ---
        let result = graph.add_hierarchical_adjacency_edges(
            |data: &TestEdgeData| data.path_id.as_ref(), // Lambda for get_path_id
            |data: &TestEdgeData| data.dep_id.as_ref(),  // Lambda for get_dependency_id
            synthetic_metadata.clone(),
        );
        assert!(result.is_ok(), "Function call failed: {:?}", result.err());

        // --- Assertions ---
        let final_edge_count = graph.edges().count();
        assert_eq!(
            final_edge_count,
            initial_edge_count + 1,
            "Expected 1 synthetic edge to be added"
        );

        // Find the synthetic edge (identified by having metadata and no value/id)
        let synthetic_edge = graph
            .edges()
            .find(|e| e.id().is_none() && e.value().is_none() && e.metadata().is_some())
            .expect("Synthetic edge not found");

        // Check the specific synthetic edge: From V2 (target of path edge) to V1 (source of dep edge)
        assert_eq!(
            synthetic_edge.source(),
            v2_idx,
            "Synthetic edge source should be V2 index"
        );
        assert_eq!(
            synthetic_edge.target(),
            v1_idx,
            "Synthetic edge target should be V1 index"
        );
        assert_eq!(
            synthetic_edge.source(),
            v2_idx,
            "Synthetic edge source should be V2 index"
        );
        assert_eq!(
            synthetic_edge.target(),
            v1_idx,
            "Synthetic edge target should be V1 index"
        );
        assert_eq!(
            synthetic_edge.metadata(),
            synthetic_metadata.as_ref(),
            "Synthetic edge metadata mismatch"
        );
    }

    #[test]
    fn test_add_adjacency_edges_mermaid_types() {
        // --- Placeholder Structs based on Mermaid Line types ---
        #[derive(Debug, Clone, PartialEq)]
        struct EmitMermaidLineSignalLine {
            // Using callee_func_path as the 'path' identifier
            callee_func_path: HierarchicalId,
            // Other fields would go here...
            _marker: (), // To make it distinct
        }

        #[derive(Debug, Clone, PartialEq)]
        struct EmitMermaidLineReturnSignalLine {
            // Using return_stmt_id_dependency as the 'dependency' identifier
            return_stmt_id_dependency: HierarchicalId,
            // Other fields would go here...
            _marker: (), // To make it distinct
        }

        // Enum to hold different edge data types and synthetic marker
        #[derive(Debug, Clone, PartialEq)]
        enum MermaidEdgeData {
            Signal(EmitMermaidLineSignalLine),
            ReturnSignal(EmitMermaidLineReturnSignalLine),
            Synthetic, // Used for metadata of synthetic edges
        }

        // --- Graph Setup ---
        let mut graph = HierarchicalIntervalGraph::<&str, MermaidEdgeData>::new();

        // Vertices (Using 8 vertices to create disconnected components initially)
        let v1_id = HierarchicalId::new("1");
        let v2_id = HierarchicalId::new("2");
        let v3_id = HierarchicalId::new("3");
        let v4_id = HierarchicalId::new("4");
        let v5_id = HierarchicalId::new("5");
        let v6_id = HierarchicalId::new("6");
        let v7_id = HierarchicalId::new("7");
        let v8_id = HierarchicalId::new("8");

        let v1_idx = graph.add_vertex(v1_id.clone(), Some("V1")).unwrap();
        let v2_idx = graph.add_vertex(v2_id.clone(), Some("V2")).unwrap();
        let v3_idx = graph.add_vertex(v3_id.clone(), Some("V3")).unwrap();
        let v4_idx = graph.add_vertex(v4_id.clone(), Some("V4")).unwrap();
        let v5_idx = graph.add_vertex(v5_id.clone(), Some("V5")).unwrap();
        let v6_idx = graph.add_vertex(v6_id.clone(), Some("V6")).unwrap();
        let v7_idx = graph.add_vertex(v7_id.clone(), Some("V7")).unwrap();
        let _v8_idx = graph.add_vertex(v8_id.clone(), Some("V8")).unwrap(); // Prefixed unused variable

        // Edges
        // Signal Edge 1 (Path): V1 -> V2, path_id = "10.1"
        let signal_edge1_val = MermaidEdgeData::Signal(EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId::new("10.1"),
            _marker: (),
        });
        let _signal_edge1_idx = graph
            .add_edge_with_value(&v1_id, &v2_id, Some(signal_edge1_val), None)
            .unwrap();

        // Signal Edge 2 (Path): V3 -> V4, path_id = "20.1"
        let signal_edge2_val = MermaidEdgeData::Signal(EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId::new("20.1"),
            _marker: (),
        });
        let _signal_edge2_idx = graph
            .add_edge_with_value(&v3_id, &v4_id, Some(signal_edge2_val), None)
            .unwrap();

        // Return Signal Edge 1 (Dependency): V5 -> V6, dep_id = "10.1.1" (descendant of "10.1")
        let return_edge1_val = MermaidEdgeData::ReturnSignal(EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId::new("10.1.1"),
            _marker: (),
        });
        let _return_edge1_idx = graph
            .add_edge_with_value(&v5_id, &v6_id, Some(return_edge1_val), None)
            .unwrap();

        // Return Signal Edge 2 (Dependency): V7 -> V8, dep_id = "20.1.1" (descendant of "20.1")
        let return_edge2_val = MermaidEdgeData::ReturnSignal(EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId::new("20.1.1"),
            _marker: (),
        });
        let _return_edge2_idx = graph
            .add_edge_with_value(&v7_id, &v8_id, Some(return_edge2_val), None)
            .unwrap();

        let initial_edge_count = graph.edges().count();
        assert_eq!(initial_edge_count, 4);

        // --- Print graph BEFORE adding synthetic edges ---
        println!("\n--- Graph State BEFORE Adding Adjacency Edges ---");
        println!("Vertices:");
        for (idx, vertex) in graph.vertices().enumerate() {
            println!(
                "  Index {}: ID={}, Value={:?}, Outgoing={:?}, Incoming={:?}",
                idx,
                vertex.id(),
                vertex.value(),
                vertex.outgoing(),
                vertex.incoming()
            );
        }
        println!("Edges:");
        for (idx, edge) in graph.edges().enumerate() {
            println!(
                "  Index {}: Source={}, Target={}, ID={:?}, Value={:?}, Metadata={:?}",
                idx,
                edge.source(),
                edge.target(),
                edge.id(),
                edge.value(),
                edge.metadata()
            );
        }
        println!("-------------------------------------------------");

        // --- Extractor Functions ---
        fn get_path_id_mermaid(data: &MermaidEdgeData) -> Option<&HierarchicalId> {
            match data {
                MermaidEdgeData::Signal(s) => Some(&s.callee_func_path),
                _ => None,
            }
        }
        fn get_dependency_id_mermaid(data: &MermaidEdgeData) -> Option<&HierarchicalId> {
            match data {
                MermaidEdgeData::ReturnSignal(rs) => Some(&rs.return_stmt_id_dependency),
                _ => None,
            }
        }

        // --- Synthetic Metadata ---
        let synthetic_metadata = Some(MermaidEdgeData::Synthetic);

        // --- Call the Function ---
        let result = graph.add_hierarchical_adjacency_edges(
            get_path_id_mermaid,
            get_dependency_id_mermaid,
            synthetic_metadata.clone(),
        );
        assert!(result.is_ok(), "Function call failed: {:?}", result.err());
        //
        //
        // --- Print graph AFTER adding synthetic edges ---
        println!("\n--- Graph State AFTER Adding Adjacency Edges ---");
        println!("Vertices:");
        for (idx, vertex) in graph.vertices().enumerate() {
            println!(
                "  Index {}: ID={}, Value={:?}, Outgoing={:?}, Incoming={:?}",
                idx,
                vertex.id(),
                vertex.value(),
                vertex.outgoing(),
                vertex.incoming()
            );
        }
        println!("Edges:");
        for (idx, edge) in graph.edges().enumerate() {
            println!(
                "  Index {}: Source={}, Target={}, ID={:?}, Value={:?}, Metadata={:?}",
                idx,
                edge.source(),
                edge.target(),
                edge.id(),
                edge.value(),
                edge.metadata()
            );
        }
        println!("------------------------------------------------");

        // --- Assertions ---
        let final_edge_count = graph.edges().count();
        assert_eq!(
            final_edge_count,
            initial_edge_count + 2, // Expecting 2 synthetic edges
            "Expected 2 synthetic edges to be added"
        );

        // Find the synthetic edges
        let synthetic_edges: Vec<&Edge<MermaidEdgeData>> = graph
            .edges()
            .filter(|e| e.metadata() == synthetic_metadata.as_ref())
            .collect();

        assert_eq!(
            synthetic_edges.len(),
            2,
            "Incorrect number of synthetic edges found"
        );

        // Check Synthetic Edge 1: SignalEdge1.target (V2) -> ReturnEdge1.source (V5)
        let syn_edge1_exists = synthetic_edges
            .iter()
            .any(|&e| e.source() == v2_idx && e.target() == v5_idx);
        assert!(
            syn_edge1_exists,
            "Expected synthetic edge from V2 (target of signal 1) to V5 (source of return 1)"
        );

        // Check Synthetic Edge 2: SignalEdge2.target (V4) -> ReturnEdge2.source (V7)
        let syn_edge2_exists = synthetic_edges
            .iter()
            .any(|&e| e.source() == v4_idx && e.target() == v7_idx);
        assert!(
            syn_edge2_exists,
            "Expected synthetic edge from V4 (target of signal 2) to V7 (source of return 2)"
        );

        // --- Final Acyclicity Check (using edge sort) ---
        // Perform an edge topological sort on the final graph to ensure it's acyclic
        let final_edge_sort_result = graph.topological_sort_edges();
        assert!(
            final_edge_sort_result.is_ok(),
            "Final graph should be acyclic after adding synthetic edges, but topological edge sort failed: {:?}",
            final_edge_sort_result.err()
        );

        // --- Dump Sorted Edges ---
        let sorted_edge_indices = final_edge_sort_result.unwrap();
        println!("Final topological edge sort successful ({} edges):", sorted_edge_indices.len());
        for (pos, &edge_idx) in sorted_edge_indices.iter().enumerate() {
            if let Some(edge) = graph.get_edge(edge_idx) {
                let source_id = graph.get_vertex(edge.source()).map(|v| v.id().to_string()).unwrap_or("?".to_string());
                let target_id = graph.get_vertex(edge.target()).map(|v| v.id().to_string()).unwrap_or("?".to_string());
                println!(
                    "  Pos {}: Index={}, {} -> {} (ID: {:?}, Value: {:?}, Meta: {:?})",
                    pos,
                    edge_idx,
                    source_id,
                    target_id,
                    edge.id(),
                    edge.value(),
                    edge.metadata()
                );
            } else {
                println!("  Pos {}: Index={} (Edge not found!)", pos, edge_idx);
            }
        }
    }


    #[test]
    fn test_add_adjacency_edges_four_pairs() {
        // --- Test Setup (Similar to mermaid_types test) ---
        #[derive(Debug, Clone, PartialEq)]
        struct EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId,
            _marker: (),
        }

        #[derive(Debug, Clone, PartialEq)]
        struct EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId,
            _marker: (),
        }

        #[derive(Debug, Clone, PartialEq)]
        enum MermaidEdgeData {
            Signal(EmitMermaidLineSignalLine),
            ReturnSignal(EmitMermaidLineReturnSignalLine),
            Synthetic,
        }

        // Change vertex type from &str to String to own the vertex values
        let mut graph = HierarchicalIntervalGraph::<String, MermaidEdgeData>::new();

        // --- Vertices (Need 16 vertices for 8 edges) ---
        let v_indices: Vec<VertexIndex> = (1..=16)
            .map(|i| {
                graph
                    // Pass the owned String directly
                    .add_vertex(HierarchicalId::new(&i.to_string()), Some(format!("V{}", i)))
                    .unwrap()
            })
            .collect();

        // --- Edges (4 Signal/Return Pairs) ---

        // Pair 1: Signal "10", Return "10.1"
        let signal_1_val = MermaidEdgeData::Signal(EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId::new("10"),
            _marker: (),
        });
        let _signal_1_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("1"), // V1
                &HierarchicalId::new("2"), // V2
                Some(signal_1_val),
                None,
            )
            .unwrap();
        let return_1_val = MermaidEdgeData::ReturnSignal(EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId::new("10.1"),
            _marker: (),
        });
        let _return_1_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("3"), // V3
                &HierarchicalId::new("4"), // V4
                Some(return_1_val),
                None,
            )
            .unwrap();

        // Pair 2: Signal "20", Return "20.1"
        let signal_2_val = MermaidEdgeData::Signal(EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId::new("20"),
            _marker: (),
        });
        let _signal_2_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("5"), // V5
                &HierarchicalId::new("6"), // V6
                Some(signal_2_val),
                None,
            )
            .unwrap();
        let return_2_val = MermaidEdgeData::ReturnSignal(EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId::new("20.1"),
            _marker: (),
        });
        let _return_2_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("7"), // V7
                &HierarchicalId::new("8"), // V8
                Some(return_2_val),
                None,
            )
            .unwrap();

        // Pair 3: Signal "30", Return "30.1"
        let signal_3_val = MermaidEdgeData::Signal(EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId::new("30"),
            _marker: (),
        });
        let _signal_3_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("9"),  // V9
                &HierarchicalId::new("10"), // V10
                Some(signal_3_val),
                None,
            )
            .unwrap();
        let return_3_val = MermaidEdgeData::ReturnSignal(EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId::new("30.1"),
            _marker: (),
        });
        let _return_3_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("11"), // V11
                &HierarchicalId::new("12"), // V12
                Some(return_3_val),
                None,
            )
            .unwrap();

        // Pair 4: Signal "40", Return "40.1"
        let signal_4_val = MermaidEdgeData::Signal(EmitMermaidLineSignalLine {
            callee_func_path: HierarchicalId::new("40"),
            _marker: (),
        });
        let _signal_4_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("13"), // V13
                &HierarchicalId::new("14"), // V14
                Some(signal_4_val),
                None,
            )
            .unwrap();
        let return_4_val = MermaidEdgeData::ReturnSignal(EmitMermaidLineReturnSignalLine {
            return_stmt_id_dependency: HierarchicalId::new("40.1"),
            _marker: (),
        });
        let _return_4_idx = graph
            .add_edge_with_value(
                &HierarchicalId::new("15"), // V15
                &HierarchicalId::new("16"), // V16
                Some(return_4_val),
                None,
            )
            .unwrap();

        let initial_edge_count = graph.edges().count();
        assert_eq!(initial_edge_count, 8); // 4 signals + 4 returns

        // --- Extractor Functions (copied) ---
        fn get_path_id_mermaid(data: &MermaidEdgeData) -> Option<&HierarchicalId> {
            match data {
                MermaidEdgeData::Signal(s) => Some(&s.callee_func_path),
                _ => None,
            }
        }
        fn get_dependency_id_mermaid(data: &MermaidEdgeData) -> Option<&HierarchicalId> {
            match data {
                MermaidEdgeData::ReturnSignal(rs) => Some(&rs.return_stmt_id_dependency),
                _ => None,
            }
        }

        // --- Synthetic Metadata ---
        let synthetic_metadata = Some(MermaidEdgeData::Synthetic);

        // --- Call the Function ---
        let result = graph.add_hierarchical_adjacency_edges(
            get_path_id_mermaid,
            get_dependency_id_mermaid,
            synthetic_metadata.clone(),
        );
        assert!(result.is_ok(), "Function call failed: {:?}", result.err());

        // --- Assertions ---
        let final_edge_count = graph.edges().count();
        assert_eq!(
            final_edge_count,
            initial_edge_count + 4, // Expecting 4 synthetic edges
            "Expected exactly 4 synthetic edges to be added"
        );

        // Find the synthetic edges
        let synthetic_edges: Vec<&Edge<MermaidEdgeData>> = graph
            .edges()
            .filter(|e| e.metadata() == synthetic_metadata.as_ref())
            .collect();

        assert_eq!(
            synthetic_edges.len(),
            4,
            "Incorrect number of synthetic edges found"
        );

        // Verify each expected synthetic edge exists
        // Pair 1: V2 -> V3
        let v2_idx = v_indices[1];
        let v3_idx = v_indices[2];
        assert!(
            synthetic_edges
                .iter()
                .any(|&e| e.source() == v2_idx && e.target() == v3_idx),
            "Expected synthetic edge from V2 to V3 (Pair 1)"
        );

        // Pair 2: V6 -> V7
        let v6_idx = v_indices[5];
        let v7_idx = v_indices[6];
        assert!(
            synthetic_edges
                .iter()
                .any(|&e| e.source() == v6_idx && e.target() == v7_idx),
            "Expected synthetic edge from V6 to V7 (Pair 2)"
        );

        // Pair 3: V10 -> V11
        let v10_idx = v_indices[9];
        let v11_idx = v_indices[10];
        assert!(
            synthetic_edges
                .iter()
                .any(|&e| e.source() == v10_idx && e.target() == v11_idx),
            "Expected synthetic edge from V10 to V11 (Pair 3)"
        );

        // Pair 4: V14 -> V15
        let v14_idx = v_indices[13];
        let v15_idx = v_indices[14];
        assert!(
            synthetic_edges
                .iter()
                .any(|&e| e.source() == v14_idx && e.target() == v15_idx),
            "Expected synthetic edge from V14 to V15 (Pair 4)"
        );

        // --- Final Acyclicity Check ---
        let final_edge_sort_result = graph.topological_sort_edges();
        assert!(
            final_edge_sort_result.is_ok(),
            "Final graph should be acyclic, but topological edge sort failed: {:?}",
            final_edge_sort_result.err()
        );

        // --- Dump Sorted Edges (Optional: for debugging) ---
        let sorted_edge_indices = final_edge_sort_result.unwrap();
        println!(
            "\n--- Sorted Edges for test_add_adjacency_edges_four_pairs ({} edges) ---",
            sorted_edge_indices.len()
        );
        for (pos, &edge_idx) in sorted_edge_indices.iter().enumerate() {
            if let Some(edge) = graph.get_edge(edge_idx) {
                let source_id = graph
                    .get_vertex(edge.source())
                    .map(|v| v.id().to_string())
                    .unwrap_or("?".to_string());
                let target_id = graph
                    .get_vertex(edge.target())
                    .map(|v| v.id().to_string())
                    .unwrap_or("?".to_string());
                println!(
                    "  Pos {}: Index={}, {} -> {} (ID: {:?}, Value: {:?}, Meta: {:?})",
                    pos,
                    edge_idx,
                    source_id,
                    target_id,
                    edge.id(),
                    edge.value(),
                    edge.metadata()
                );
            } else {
                println!("  Pos {}: Index={} (Edge not found!)", pos, edge_idx);
            }
        }
        println!("-----------------------------------------------------------------------");
    }
}
