// use regex::Regex;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
/// # Hydrate Module
///
/// The hydrate module sits at the very end of the code generation workflow, collecting and organizing
/// all the code fragments generated via DDlog. It represents the final stage in the pipeline where
/// raw facts from DDlog are transformed into structured, ordered output ready for consumption.
///
/// ## Workflow Position
///
/// ```text
/// +-------------+     +------------+     +-------------+     +------------+
/// | Input Data  | --> | DDlog      | --> | DDlog Drain | --> | Hydrate    | --> Final Output
/// | (AST, etc.) |     | Processing |     | (Parsing)   |     | (Ordering) |
/// +-------------+     +------------+     +-------------+     +------------+
/// ```
///
/// ## Purpose
///
/// This module provides functionality to:
///
/// 1. Collect facts from DDlog output (parsed by ddlog_drain)
/// 2. Organize these facts into logical buckets based on their relation names
/// 3. Order facts within each bucket using numerical ordering of path attributes
/// 4. Group buckets into pools based on relation name patterns
/// 5. Prioritize buckets according to user-defined relation priorities
/// 6. Generate final output by dumping pools and their buckets in priority order
///
/// The hydration process ensures that code fragments are assembled in the correct order,
/// respecting dependencies and structural requirements of the generated code.
///
/// ## Key Concepts
///
/// - **Pool**: Top-level container grouping related buckets
/// - **Buckets**: Collections of facts grouped by relation name
/// - **Ordering**: Path-based lexicographical ordering within buckets
/// - **Priorities**: User-defined importance of different relation types
/// - **Hydration**: The process of organizing and assembling facts into coherent output
///
use std::collections::{BTreeMap, HashMap};
use std::error::Error;
use std::fmt;
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::ddlog_drain::{AttributeValue, DdlogDrain, DdlogDrainError, DdlogFact};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputSource {
    pub relation_name: String,
    pub priority: u32,
}

impl InputSource {
    pub fn new(relation_name: &str, priority: u32) -> Self {
        InputSource {
            relation_name: relation_name.to_string(),
            priority,
        }
    }
}

// Helper function to compare path segments numerically
fn compare_path_segments(path1: &str, path2: &str) -> Ordering {
    let segments1: Vec<&str> = path1.split('.').collect();
    let segments2: Vec<&str> = path2.split('.').collect();

    for (i, seg1) in segments1.iter().enumerate() {
        // If we've run out of segments in path2, path1 is longer, so it's greater
        if i >= segments2.len() {
            return Ordering::Greater;
        }

        let seg2 = segments2[i];

        // Try to parse both segments as numbers
        let num1 = seg1.parse::<u64>();
        let num2 = seg2.parse::<u64>();

        match (num1, num2) {
            (Ok(n1), Ok(n2)) => {
                if n1 != n2 {
                    return n1.cmp(&n2);
                }
            }
            _ => {
                if seg1 != &seg2 {
                    return seg1.cmp(&seg2);
                }
            }
        }
    }

    // If we get here, all segments in path1 matched path2 up to path1's length
    // If path2 is longer, path1 is less, otherwise they're equal
    if segments1.len() < segments2.len() {
        Ordering::Less
    } else {
        Ordering::Equal
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BucketConfig {
    #[serde(default)]
    priorities: HashMap<String, u32>,
    #[serde(default)]
    path_attributes: HashMap<String, String>,
    #[serde(default)]
    value_attributes: HashMap<String, String>,
    #[serde(default = "default_path_attribute")]
    default_path_attribute: String,
    #[serde(default = "default_value_attribute")]
    default_value_attribute: String,
    #[serde(default)]
    default_priority: u32,
    #[serde(default = "default_pool_shape")]
    pool_shape: String,
    #[serde(default)]
    bucket_relations: HashMap<String, Vec<InputSource>>,
    #[serde(default)]
    relation_to_bucket: HashMap<String, String>,
    #[serde(default)]
    relation_priorities: HashMap<String, u32>,
    #[serde(default)]
    streams: HashMap<String, Vec<String>>,
    #[serde(default)]
    stream_shapes: HashMap<String, String>,
}

// Helper functions for serde default values
fn default_path_attribute() -> String {
    "path".to_string()
}

fn default_value_attribute() -> String {
    "val".to_string()
}

fn default_pool_shape() -> String {
    "default".to_string()
}

impl BucketConfig {
    pub fn new() -> Self {
        BucketConfig {
            priorities: HashMap::new(),
            path_attributes: HashMap::new(),
            value_attributes: HashMap::new(),
            default_path_attribute: "path".to_string(),
            default_value_attribute: "val".to_string(),
            default_priority: 0,
            pool_shape: "default".to_string(),
            bucket_relations: HashMap::new(),
            relation_to_bucket: HashMap::new(),
            relation_priorities: HashMap::new(),
            streams: HashMap::new(),
            stream_shapes: HashMap::new(),
        }
    }

    /// Create a new Hydrator from a YAML configuration file
    ///
    /// # Arguments
    ///
    /// * `file_path` - Path to the YAML configuration file
    ///
    /// # Returns
    ///
    /// * `Result<Hydrator, Box<dyn Error>>` - The created Hydrator or an error
    ///
    /// # Example
    ///
    /// let config = BucketConfig::from_yaml_file("config.yaml")?;
    /// let hydrator = Hydrator::new(config);
    /// ```
    pub fn from_yaml_file<P: AsRef<Path>>(file_path: P) -> Result<Self, Box<dyn Error>> {
        let mut file = File::open(file_path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let mut config: BucketConfig = serde_yaml::from_str(&contents)?;

        // Process the loaded configuration to build the derived maps
        config.rebuild_derived_maps();

        Ok(config)
    }

    pub fn to_yaml_file<P: AsRef<Path>>(&self, file_path: P) -> Result<(), Box<dyn Error>> {
        let yaml = serde_yaml::to_string(self)?;
        std::fs::write(file_path, yaml)?;
        Ok(())
    }

    ///
    /// Rebuild the derived maps (relation_to_bucket and relation_priorities)
    /// This is needed after loading from YAML to ensure all internal state is consistent
    fn rebuild_derived_maps(&mut self) {
        self.relation_to_bucket.clear();
        self.relation_priorities.clear();

        // Rebuild relation_to_bucket and relation_priorities maps from bucket_relations
        for (bucket_name, input_sources) in &self.bucket_relations {
            for source in input_sources {
                self.relation_to_bucket
                    .insert(source.relation_name.clone(), bucket_name.clone());
                self.relation_priorities
                    .insert(source.relation_name.clone(), source.priority);
            }
        }
    }

    /// Load a BucketConfig from a YAML file
    ///
    /// # Arguments
    ///
    /// * `file_path` - Path to the YAML configuration file
    ///
    /// # Returns
    ///
    /// * `Result<BucketConfig, Box<dyn Error>>` - The loaded configuration or an error
    ///
    /// # Example
    ///
    ///
    ///
    /// Single entry point for configuring buckets
    ///
    /// This method allows setting all bucket properties in one call:
    /// - bucket_name: Name of the bucket
    /// - priority: Priority of the bucket (higher values are processed first)
    /// - path_attribute: Attribute name used for path-based ordering
    /// - value_attribute: Attribute name used for values
    /// - relations: List of relation names that should be placed in this bucket
    ///
    pub fn with_bucket(
        mut self,
        bucket_name: &str,
        priority: u32,
        path_attribute: &str,
        value_attribute: &str,
        input_sources: Vec<InputSource>,
    ) -> Self {
        // Set bucket priority
        self.priorities.insert(bucket_name.to_string(), priority);

        // Set bucket path attribute
        self.path_attributes
            .insert(bucket_name.to_string(), path_attribute.to_string());

        // Set bucket value attribute
        self.value_attributes
            .insert(bucket_name.to_string(), value_attribute.to_string());

        // Store the input sources for this bucket
        self.bucket_relations
            .insert(bucket_name.to_string(), input_sources.clone());

        // Map each relation to this bucket and store relation priorities
        for source in input_sources {
            self.relation_to_bucket
                .insert(source.relation_name.clone(), bucket_name.to_string());
            self.relation_priorities
                .insert(source.relation_name.clone(), source.priority);
        }

        self
    }

    // Convenience method to create InputSource objects from relation names with the same priority
    pub fn with_bucket_simple(
        self,
        bucket_name: &str,
        priority: u32,
        path_attribute: &str,
        value_attribute: &str,
        relations: Vec<&str>,
    ) -> Self {
        let input_sources: Vec<InputSource> = relations
            .iter()
            .map(|r| InputSource::new(r, priority))
            .collect();

        self.with_bucket(
            bucket_name,
            priority,
            path_attribute,
            value_attribute,
            input_sources,
        )
    }

    pub fn with_default_path_attribute(mut self, attribute: &str) -> Self {
        self.default_path_attribute = attribute.to_string();
        self
    }

    pub fn with_default_value_attribute(mut self, attribute: &str) -> Self {
        self.default_value_attribute = attribute.to_string();
        self
    }

    pub fn with_default_priority(mut self, priority: u32) -> Self {
        self.default_priority = priority;
        self
    }

    pub fn with_pool_shape(mut self, shape: &str) -> Self {
        self.pool_shape = shape.to_string();
        self
    }

    /// Define a stream that merges multiple buckets
    ///
    /// # Arguments
    ///
    /// * `stream_name` - Name of the stream
    /// * `bucket_names` - List of bucket names to include in the stream
    ///
    /// # Returns
    ///
    /// * `Self` - The updated configuration
    pub fn with_stream(mut self, stream_name: &str, bucket_names: Vec<&str>) -> Self {
        let bucket_names: Vec<String> = bucket_names.iter().map(|&s| s.to_string()).collect();

        // Ensure we have at least 2 buckets
        if bucket_names.len() >= 2 {
            self.streams.insert(stream_name.to_string(), bucket_names);
            // Use default shape
            self.stream_shapes
                .insert(stream_name.to_string(), "default".to_string());
        }

        self
    }

    /// Define a stream with a custom shape
    ///
    /// # Arguments
    ///
    /// * `stream_name` - Name of the stream
    /// * `bucket_names` - List of bucket names to include in the stream
    /// * `shape` - Shape of the stream
    ///
    /// # Returns
    ///
    /// * `Self` - The updated configuration
    pub fn with_stream_shape(
        mut self,
        stream_name: &str,
        bucket_names: Vec<&str>,
        shape: &str,
    ) -> Self {
        let bucket_names: Vec<String> = bucket_names.iter().map(|&s| s.to_string()).collect();

        // Ensure we have at least 2 buckets
        if bucket_names.len() >= 2 {
            self.streams.insert(stream_name.to_string(), bucket_names);
            self.stream_shapes
                .insert(stream_name.to_string(), shape.to_string());
        }

        self
    }
}

impl Default for BucketConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct OrderedFact {
    fact: DdlogFact,
    path: String,
    relation_priority: u32,
}

impl OrderedFact {
    fn new(fact: DdlogFact, path_attribute: &str, relation_priority: u32) -> Self {
        let path = fact
            .attributes
            .get(path_attribute)
            .map(|attr| match attr {
                // Prioritize Path type for path attributes
                AttributeValue::Path(p) => p.clone(),
                AttributeValue::String(s) => s.clone(),
                AttributeValue::Number(n) => n.to_string(),
            })
            .unwrap_or_default();

        OrderedFact {
            fact,
            path,
            relation_priority,
        }
    }
}

impl PartialEq for OrderedFact {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Eq for OrderedFact {}

impl PartialOrd for OrderedFact {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for OrderedFact {
    fn cmp(&self, other: &Self) -> Ordering {
        // First compare by path
        match compare_path_segments(&self.path, &other.path) {
            Ordering::Equal => {
                // If paths are equal, compare by relation priority (higher priority comes first)
                other.relation_priority.cmp(&self.relation_priority)
            }
            ordering => ordering,
        }
    }
}

#[derive(Debug, Clone)]
struct Bucket {
    // The bucket name, which may represent multiple relations
    name: String,
    // The original relation names that contributed facts to this bucket
    relations: Vec<String>,
    config: BucketConfig,
    facts: Vec<OrderedFact>,
}

impl Bucket {
    fn new(name: String, config: BucketConfig) -> Self {
        Bucket {
            name,
            relations: Vec::new(),
            config,
            facts: Vec::new(),
        }
    }

    fn priority(&self) -> u32 {
        // First try to get priority for the bucket name
        if let Some(priority) = self.config.priorities.get(&self.name) {
            return *priority;
        }
        // Fall back to default priority
        self.config.default_priority
    }

    fn path_attribute(&self) -> String {
        // First try to get path attribute for the bucket name
        if let Some(path_attr) = self.config.path_attributes.get(&self.name) {
            return path_attr.clone();
        }
        // Fall back to default path attribute
        self.config.default_path_attribute.clone()
    }

    fn value_attribute(&self) -> String {
        // First try to get value attribute for the bucket name
        if let Some(value_attr) = self.config.value_attributes.get(&self.name) {
            return value_attr.clone();
        }
        // Fall back to default value attribute
        self.config.default_value_attribute.clone()
    }

    fn add_fact(&mut self, fact: DdlogFact) {
        // Add the relation to the list of relations if it's not already there
        let relation_name = fact.relation_name.clone();
        if !self.relations.contains(&relation_name) {
            self.relations.push(relation_name.clone());
        }

        // Get the relation priority
        let relation_priority = self
            .config
            .relation_priorities
            .get(&relation_name)
            .cloned()
            .unwrap_or(self.config.default_priority);

        // Create an OrderedFact using the bucket's path attribute and relation priority
        let path_attr = self.path_attribute();
        let ordered_fact = OrderedFact::new(fact, &path_attr, relation_priority);

        // Insert the fact and then sort the vector to maintain ordering
        self.facts.push(ordered_fact);
        self.facts.sort(); // This uses the Ord implementation for OrderedFact
    }

    fn dump(&self) -> String {
        let mut result = String::new();

        result.push_str(&format!(
            "%% Bucket: {} (priority: {}, path_attribute: {}, value_attribute: {})
",
            self.name,
            self.priority(),
            self.path_attribute(),
            self.value_attribute()
        ));

        // List all relations that contributed to this bucket
        if !self.relations.is_empty() {
            result.push_str("%% Relations: ");
            for (i, relation) in self.relations.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                let priority = self
                    .config
                    .relation_priorities
                    .get(relation)
                    .cloned()
                    .unwrap_or(self.config.default_priority);
                result.push_str(&format!("{}(priority: {})", relation, priority));
            }
            result.push_str("\n");
        }

        for fact in &self.facts {
            let value_attr = self.value_attribute();
            let value = fact
                .fact
                .attributes
                .get(&value_attr)
                .map_or("", |v| match v {
                    AttributeValue::String(s) => s,
                    AttributeValue::Path(p) => p,
                    AttributeValue::Number(_) => "", // Skip numbers as values
                });

            let fact_path = if !fact.path.is_empty() {
                &fact.path
            } else {
                "unspecified"
            };

            result.push_str(&format!(
                "%% Fact: (relation: {}, priority: {}, path: {})\n{}\n",
                fact.fact.relation_name, fact.relation_priority, fact_path, value
            ));
        }

        result
    }
}

#[derive(Debug)]
pub struct Pool {
    shape: String,
    buckets: Vec<Bucket>,
}

impl Pool {
    fn new(shape: String) -> Self {
        Pool {
            shape,
            buckets: Vec::new(),
        }
    }

    fn add_bucket(&mut self, bucket: Bucket) {
        self.buckets.push(bucket);
    }

    fn sort_buckets_by_priority(&mut self) {
        self.buckets.sort_by(|a, b| b.priority().cmp(&a.priority()));
    }
}

impl Display for Pool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "%% Pool (shape: {})", self.shape)?;
        writeln!(f, "{}", self.shape)?;

        for bucket in &self.buckets {
            write!(f, "{}", bucket.dump())?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Stream {
    name: String,
    bucket_names: Vec<String>,
    shape: String,
}

impl Stream {
    fn new(name: String, bucket_names: Vec<String>, shape: String) -> Self {
        Stream {
            name,
            bucket_names,
            shape,
        }
    }

    fn collect_facts<'a>(&'a self, buckets: &'a HashMap<String, Bucket>) -> Vec<&'a OrderedFact> {
        let mut all_facts = Vec::new();

        // Collect facts from all buckets in this stream
        for bucket_name in &self.bucket_names {
            if let Some(bucket) = buckets.get(bucket_name) {
                for fact in &bucket.facts {
                    all_facts.push(fact);
                }
            }
        }

        // Sort all facts by path
        all_facts.sort_by(|a, b| compare_path_segments(&a.path, &b.path));

        all_facts
    }

    fn dump<'a>(&'a self, buckets: &'a HashMap<String, Bucket>) -> String {
        let mut result = String::new();

        result.push_str(&format!(
            "%% Stream: {} (shape: {}, buckets: {:?})\n",
            self.name, self.shape, self.bucket_names
        ));

        // Output the stream shape first
        result.push_str(&format!("{}\n", self.shape));

        // Collect and sort all facts from the buckets in this stream
        let all_facts = self.collect_facts(buckets);

        // Output the facts in order
        for fact in all_facts {
            let bucket_name = fact.fact.relation_name.clone();
            let bucket = buckets.get(&bucket_name);

            let value_attr = bucket.map_or("val".to_string(), |b| b.value_attribute());
            let value = fact
                .fact
                .attributes
                .get(&value_attr)
                .map_or("", |v| match v {
                    AttributeValue::String(s) => s,
                    AttributeValue::Path(p) => p,
                    AttributeValue::Number(_) => "", // Skip numbers as values
                });

            result.push_str(&format!(
                "%% Stream Fact: (bucket: {}, path: {})\n{}\n",
                bucket_name, fact.path, value
            ));
        }

        result
    }
}

#[derive(Debug)]
pub struct Hydrator {
    global_config: BucketConfig,
    buckets: HashMap<String, Bucket>,
    streams: HashMap<String, Stream>,
}

impl Hydrator {
    pub fn new(config: BucketConfig) -> Self {
        // Initialize streams from config
        let mut streams = HashMap::new();
        for (stream_name, bucket_names) in &config.streams {
            let shape = config
                .stream_shapes
                .get(stream_name)
                .cloned()
                .unwrap_or_else(|| "default".to_string());

            streams.insert(
                stream_name.clone(),
                Stream::new(stream_name.clone(), bucket_names.clone(), shape),
            );
        }

        Hydrator {
            global_config: config,
            buckets: HashMap::new(),
            streams,
        }
    }

    pub fn process_fact(&mut self, fact: DdlogFact) {
        let relation = fact.relation_name.clone();

        // Determine which bucket this relation belongs to
        let bucket_name = if let Some(bucket) = self.global_config.relation_to_bucket.get(&relation)
        {
            // This relation is mapped to a specific bucket
            bucket.clone()
        } else if self.global_config.priorities.contains_key(&relation) {
            // This relation has its own priority, so it gets its own bucket
            relation.clone()
        } else {
            // Unknown relation, ignore it
            return;
        };

        // Get or create the bucket
        let bucket = self
            .buckets
            .entry(bucket_name.clone())
            .or_insert_with(|| Bucket::new(bucket_name, self.global_config.clone()));

        // Add the fact to the bucket
        bucket.add_fact(fact);
    }

    pub fn process_drain<I>(&mut self, drain: DdlogDrain<I>)
    where
        I: Iterator<Item = String>,
    {
        for result in drain {
            match result {
                Ok(fact) => {
                    // Check if the relation is known before processing
                    let relation_name = &fact.relation_name;
                    let is_known = self.global_config.priorities.contains_key(relation_name)
                        || self
                            .global_config
                            .relation_to_bucket
                            .contains_key(relation_name);

                    if is_known {
                        self.process_fact(fact);
                    }
                }
                Err(e) => eprintln!("Error processing fact: {}", e),
            }
        }
    }

    pub fn create_pool(&self, shape: String) -> Pool {
        let mut pool = Pool::new(shape);

        // Clone buckets and add them to the pool
        for bucket in self.buckets.values() {
            pool.add_bucket(bucket.clone());
        }

        // Sort buckets by priority
        pool.sort_buckets_by_priority();

        pool
    }

    pub fn dump(&self) -> String {
        let pool = self.create_pool(self.global_config.pool_shape.clone());
        format!("{}", pool)
    }

    pub fn buckets(&self) -> &HashMap<String, Bucket> {
        &self.buckets
    }

    pub fn clear(&mut self) {
        self.buckets.clear();
    }

    /// Get a reference to the streams
    pub fn streams(&self) -> &HashMap<String, Stream> {
        &self.streams
    }

    /// Dump a specific stream by name
    pub fn dump_stream(&self, stream_name: &str) -> Option<String> {
        self.streams
            .get(stream_name)
            .map(|stream| stream.dump(&self.buckets))
    }

    /// Dump all streams
    pub fn dump_streams(&self) -> String {
        let mut result = String::new();

        for stream in self.streams.values() {
            result.push_str(&stream.dump(&self.buckets));
            result.push_str("\n");
        }

        result
    }

    /// Add a new stream
    pub fn add_stream(&mut self, stream_name: &str, bucket_names: Vec<String>) {
        if bucket_names.len() >= 2 {
            self.streams.insert(
                stream_name.to_string(),
                Stream::new(stream_name.to_string(), bucket_names, "default".to_string()),
            );
        }
    }

    /// Add a new stream with a custom shape
    pub fn add_stream_with_shape(
        &mut self,
        stream_name: &str,
        bucket_names: Vec<String>,
        shape: &str,
    ) {
        if bucket_names.len() >= 2 {
            self.streams.insert(
                stream_name.to_string(),
                Stream::new(stream_name.to_string(), bucket_names, shape.to_string()),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ddlog_drain::{AttributeValue, DdlogDrain};
    use std::collections::HashMap;

    #[test]
    fn test_bucket_config_default_attributes() {
        // Test that the with_default_path_attribute and with_default_value_attribute methods work
        let config = BucketConfig::new()
            .with_default_path_attribute("custom_path")
            .with_default_value_attribute("custom_value");

        assert_eq!(config.default_path_attribute, "custom_path");
        assert_eq!(config.default_value_attribute, "custom_value");
    }

    #[test]
    fn test_bucket_config_default_priority() {
        // Test that the with_default_priority method works
        let config = BucketConfig::new().with_default_priority(100);

        assert_eq!(config.default_priority, 100);
    }

    #[test]
    fn test_bucket_config_pool_shape() {
        // Test that the with_pool_shape method works
        let config = BucketConfig::new().with_pool_shape("custom_shape");

        assert_eq!(config.pool_shape, "custom_shape");
    }

    #[test]
    fn test_bucket_config_with_stream() {
        // Test that the with_stream method works
        let config = BucketConfig::new().with_stream("test_stream", vec!["bucket1", "bucket2"]);

        assert!(config.streams.contains_key("test_stream"));
        let buckets = config.streams.get("test_stream").unwrap();
        assert_eq!(buckets.len(), 2);
        assert_eq!(buckets[0], "bucket1");
        assert_eq!(buckets[1], "bucket2");

        // Check default shape
        assert_eq!(config.stream_shapes.get("test_stream").unwrap(), "default");
    }

    #[test]
    fn test_bucket_config_with_stream_shape() {
        // Test that the with_stream_shape method works
        let config = BucketConfig::new().with_stream_shape(
            "test_stream",
            vec!["bucket1", "bucket2"],
            "custom_shape",
        );

        assert!(config.streams.contains_key("test_stream"));
        let buckets = config.streams.get("test_stream").unwrap();
        assert_eq!(buckets.len(), 2);
        assert_eq!(buckets[0], "bucket1");
        assert_eq!(buckets[1], "bucket2");

        // Check custom shape
        assert_eq!(
            config.stream_shapes.get("test_stream").unwrap(),
            "custom_shape"
        );
    }

    #[test]
    fn test_stream_collect_facts() {
        // Create a test configuration
        let config = BucketConfig::new()
            .with_bucket_simple("bucket1", 100, "path", "val", vec!["relation1"])
            .with_bucket_simple("bucket2", 200, "path", "val", vec!["relation2"])
            .with_stream_shape("test_stream", vec!["bucket1", "bucket2"], "test_shape");

        // Create a hydrator
        let mut hydrator = Hydrator::new(config);

        // Create some test facts
        let mut fact1 = DdlogFact {
            relation_name: "relation1".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        fact1
            .attributes
            .insert("path".to_string(), AttributeValue::Path("1.2".to_string()));
        fact1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("value1".to_string()),
        );

        let mut fact2 = DdlogFact {
            relation_name: "relation2".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        fact2
            .attributes
            .insert("path".to_string(), AttributeValue::Path("1.1".to_string()));
        fact2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("value2".to_string()),
        );

        // Process the facts
        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);

        // Get the stream output
        let stream_output = hydrator.dump_stream("test_stream").unwrap();

        // The output should contain both facts, with fact2 before fact1 due to path ordering
        assert!(stream_output.contains("test_shape"));
        assert!(stream_output.contains("value1"));
        assert!(stream_output.contains("value2"));

        // Check if the facts are in the correct order (path 1.1 before 1.2)
        let value1_pos = stream_output.find("value1").unwrap();
        let value2_pos = stream_output.find("value2").unwrap();
        assert!(value2_pos < value1_pos);
    }

    #[test]
    fn test_different_path_attributes_in_stream() {
        let config = BucketConfig::new()
            .with_bucket_simple("bucket_a", 100, "path_a", "val", vec!["relation_a"])
            .with_bucket_simple("bucket_b", 100, "path_b", "val", vec!["relation_b"])
            .with_stream_shape("mixed_stream", vec!["bucket_a", "bucket_b"], "mixed_shape");

        let mut hydrator = Hydrator::new(config);

        let mut fact_a1 = DdlogFact {
            relation_name: "relation_a".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        fact_a1
            .attributes
            .insert("path_a".to_string(), AttributeValue::Path("2".to_string()));
        fact_a1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("value_a1".to_string()),
        );

        let mut fact_a2 = DdlogFact {
            relation_name: "relation_a".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        fact_a2
            .attributes
            .insert("path_a".to_string(), AttributeValue::Path("1".to_string()));
        fact_a2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("value_a2".to_string()),
        );

        let mut fact_b1 = DdlogFact {
            relation_name: "relation_b".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        fact_b1
            .attributes
            .insert("path_b".to_string(), AttributeValue::Path("4".to_string()));
        fact_b1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("value_b1".to_string()),
        );

        let mut fact_b2 = DdlogFact {
            relation_name: "relation_b".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        fact_b2
            .attributes
            .insert("path_b".to_string(), AttributeValue::Path("3".to_string()));
        fact_b2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("value_b2".to_string()),
        );

        hydrator.process_fact(fact_a1);
        hydrator.process_fact(fact_b1);
        hydrator.process_fact(fact_a2);
        hydrator.process_fact(fact_b2);

        let stream_output = hydrator.dump_stream("mixed_stream").unwrap();

        assert!(stream_output.contains("mixed_shape"));

        let pos_a2 = stream_output.find("value_a2").unwrap();
        let pos_a1 = stream_output.find("value_a1").unwrap();
        let pos_b2 = stream_output.find("value_b2").unwrap();
        let pos_b1 = stream_output.find("value_b1").unwrap();

        assert!(
            pos_a2 < pos_a1,
            "Facts from bucket_a should be ordered by path_a"
        );
        assert!(
            pos_b2 < pos_b1,
            "Facts from bucket_b should be ordered by path_b"
        );
        assert!(
            pos_a1 < pos_b2,
            "All facts should be ordered numerically by path"
        );

        assert!(stream_output.contains("path: 1"));
        assert!(stream_output.contains("path: 2"));
        assert!(stream_output.contains("path: 3"));
        assert!(stream_output.contains("path: 4"));
    }

    #[test]
    fn test_complex_stream_ordering() {
        let config = BucketConfig::new()
            .with_bucket_simple("imports", 100, "path", "val", vec!["import_relation"])
            .with_bucket_simple("functions", 90, "path", "val", vec!["function_relation"])
            .with_bucket_simple("classes", 80, "path", "val", vec!["class_relation"])
            .with_bucket_simple("methods", 70, "path", "val", vec!["method_relation"])
            .with_stream_shape(
                "code_stream",
                vec!["imports", "functions", "classes", "methods"],
                "code_shape",
            )
            .with_stream_shape("api_stream", vec!["functions", "methods"], "api_shape");

        let mut hydrator = Hydrator::new(config);

        let mut import1 = DdlogFact {
            relation_name: "import_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        import1
            .attributes
            .insert("path".to_string(), AttributeValue::Path("1".to_string()));
        import1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("import os".to_string()),
        );

        let mut import2 = DdlogFact {
            relation_name: "import_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        import2
            .attributes
            .insert("path".to_string(), AttributeValue::Path("2".to_string()));
        import2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("import sys".to_string()),
        );

        let mut func1 = DdlogFact {
            relation_name: "function_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        func1
            .attributes
            .insert("path".to_string(), AttributeValue::Path("10.1".to_string()));
        func1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("def func1(): pass".to_string()),
        );

        let mut func2 = DdlogFact {
            relation_name: "function_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        func2
            .attributes
            .insert("path".to_string(), AttributeValue::Path("10.2".to_string()));
        func2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("def func2(): pass".to_string()),
        );

        let mut class1 = DdlogFact {
            relation_name: "class_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        class1
            .attributes
            .insert("path".to_string(), AttributeValue::Path("20.1".to_string()));
        class1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("class Class1: pass".to_string()),
        );

        let mut class2 = DdlogFact {
            relation_name: "class_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        class2
            .attributes
            .insert("path".to_string(), AttributeValue::Path("20.2".to_string()));
        class2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("class Class2: pass".to_string()),
        );

        let mut method1 = DdlogFact {
            relation_name: "method_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        method1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("20.1.1".to_string()),
        );
        method1.attributes.insert(
            "val".to_string(),
            AttributeValue::String("def method1(self): pass".to_string()),
        );

        let mut method2 = DdlogFact {
            relation_name: "method_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        method2.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("20.1.2".to_string()),
        );
        method2.attributes.insert(
            "val".to_string(),
            AttributeValue::String("def method2(self): pass".to_string()),
        );

        let mut method3 = DdlogFact {
            relation_name: "method_relation".to_string(),
            attributes: HashMap::new(),
            diff: None,
        };
        method3.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("20.2.1".to_string()),
        );
        method3.attributes.insert(
            "val".to_string(),
            AttributeValue::String("def method3(self): pass".to_string()),
        );

        hydrator.process_fact(method2.clone());
        hydrator.process_fact(import2.clone());
        hydrator.process_fact(class1.clone());
        hydrator.process_fact(func2.clone());
        hydrator.process_fact(method1.clone());
        hydrator.process_fact(import1.clone());
        hydrator.process_fact(class2.clone());
        hydrator.process_fact(func1.clone());
        hydrator.process_fact(method3.clone());

        let code_stream_output = hydrator.dump_stream("code_stream").unwrap();

        assert!(code_stream_output.contains("code_shape"));
        assert!(code_stream_output.contains("import os"));
        assert!(code_stream_output.contains("import sys"));

        assert!(code_stream_output.contains("def func1(): pass"));
        assert!(code_stream_output.contains("def func2(): pass"));

        assert!(code_stream_output.contains("class Class1: pass"));
        assert!(code_stream_output.contains("class Class2: pass"));

        assert!(code_stream_output.contains("def method1(self): pass"));
        assert!(code_stream_output.contains("def method2(self): pass"));
        assert!(code_stream_output.contains("def method3(self): pass"));

        let import_os_pos = code_stream_output.find("import os").unwrap();
        let import_sys_pos = code_stream_output.find("import sys").unwrap();
        let func1_pos = code_stream_output.find("def func1(): pass").unwrap();
        let func2_pos = code_stream_output.find("def func2(): pass").unwrap();
        let class1_pos = code_stream_output.find("class Class1: pass").unwrap();
        let class2_pos = code_stream_output.find("class Class2: pass").unwrap();
        let method1_pos = code_stream_output.find("def method1(self): pass").unwrap();
        let method2_pos = code_stream_output.find("def method2(self): pass").unwrap();
        let method3_pos = code_stream_output.find("def method3(self): pass").unwrap();

        assert!(import_os_pos < import_sys_pos);
        assert!(import_sys_pos < func1_pos);
        assert!(func1_pos < func2_pos);
        assert!(func2_pos < class1_pos);
        assert!(class1_pos < class2_pos);
        assert!(class1_pos < method1_pos);
        assert!(class2_pos < method3_pos);
        assert!(method1_pos < method2_pos);
        assert!(method2_pos < method3_pos);

        let api_stream_output = hydrator.dump_stream("api_stream").unwrap();

        assert!(api_stream_output.contains("api_shape"));
        assert!(!api_stream_output.contains("import os"));
        assert!(!api_stream_output.contains("import sys"));
        assert!(!api_stream_output.contains("class Class1: pass"));
        assert!(!api_stream_output.contains("class Class2: pass"));
        assert!(api_stream_output.contains("def func1(): pass"));
        assert!(api_stream_output.contains("def func2(): pass"));
        assert!(api_stream_output.contains("def method1(self): pass"));
        assert!(api_stream_output.contains("def method2(self): pass"));
        assert!(api_stream_output.contains("def method3(self): pass"));

        let api_func1_pos = api_stream_output.find("def func1(): pass").unwrap();
        let api_func2_pos = api_stream_output.find("def func2(): pass").unwrap();
        let api_method1_pos = api_stream_output.find("def method1(self): pass").unwrap();
        let api_method2_pos = api_stream_output.find("def method2(self): pass").unwrap();
        let api_method3_pos = api_stream_output.find("def method3(self): pass").unwrap();

        assert!(api_func1_pos < api_func2_pos);
        assert!(api_method1_pos < api_method2_pos);
        assert!(api_method2_pos < api_method3_pos);

        hydrator.add_stream_with_shape(
            "doc_stream",
            vec!["functions".to_string(), "classes".to_string()],
            "doc_shape",
        );

        let doc_stream_output = hydrator.dump_stream("doc_stream").unwrap();

        assert!(doc_stream_output.contains("doc_shape"));
        assert!(!doc_stream_output.contains("import os"));
        assert!(!doc_stream_output.contains("import sys"));
        assert!(!doc_stream_output.contains("def method1(self): pass"));
        assert!(!doc_stream_output.contains("def method2(self): pass"));
        assert!(!doc_stream_output.contains("def method3(self): pass"));
        assert!(doc_stream_output.contains("def func1(): pass"));
        assert!(doc_stream_output.contains("def func2(): pass"));
        assert!(doc_stream_output.contains("class Class1: pass"));
        assert!(doc_stream_output.contains("class Class2: pass"));

        let doc_func1_pos = doc_stream_output.find("def func1(): pass").unwrap();
        let doc_func2_pos = doc_stream_output.find("def func2(): pass").unwrap();
        let doc_class1_pos = doc_stream_output.find("class Class1: pass").unwrap();
        let doc_class2_pos = doc_stream_output.find("class Class2: pass").unwrap();

        assert!(doc_func1_pos < doc_func2_pos);
        assert!(doc_class1_pos < doc_class2_pos);
        assert!(doc_func2_pos < doc_class1_pos);
    }

    #[test]
    fn test_bucket_config_from_yaml_file() {
        // Test loading a BucketConfig from a YAML file
        let config_path = "src/test_data/test_config.yaml";
        let config = BucketConfig::from_yaml_file(config_path).unwrap();

        // Verify default values
        assert_eq!(config.default_path_attribute, "custom_path");
        assert_eq!(config.default_value_attribute, "custom_value");
        assert_eq!(config.default_priority, 50);
        assert_eq!(config.pool_shape, "test_pool");

        // Verify bucket priorities
        assert_eq!(*config.priorities.get("bucket1").unwrap(), 100);
        assert_eq!(*config.priorities.get("bucket2").unwrap(), 200);

        // Verify path attributes
        assert_eq!(config.path_attributes.get("bucket1").unwrap(), "path1");
        assert_eq!(config.path_attributes.get("bucket2").unwrap(), "path2");

        // Verify value attributes
        assert_eq!(config.value_attributes.get("bucket1").unwrap(), "val1");
        assert_eq!(config.value_attributes.get("bucket2").unwrap(), "val2");

        // Verify relation mappings were correctly built
        assert_eq!(
            config.relation_to_bucket.get("relation1").unwrap(),
            "bucket1"
        );
        assert_eq!(
            config.relation_to_bucket.get("relation2").unwrap(),
            "bucket1"
        );
        assert_eq!(
            config.relation_to_bucket.get("relation3").unwrap(),
            "bucket2"
        );

        // Verify relation priorities were correctly built
        assert_eq!(*config.relation_priorities.get("relation1").unwrap(), 10);
        assert_eq!(*config.relation_priorities.get("relation2").unwrap(), 20);
        assert_eq!(*config.relation_priorities.get("relation3").unwrap(), 30);
    }
}
