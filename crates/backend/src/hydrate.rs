use regex::Regex;
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
use std::fmt;
use std::fmt::Display;

use crate::ddlog_drain::{AttributeValue, DdlogDrain, DdlogDrainError, DdlogFact};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BucketConfig {
    priorities: HashMap<String, u32>,
    path_attributes: HashMap<String, String>,
    value_attributes: HashMap<String, String>,
    default_path_attribute: String,
    default_value_attribute: String,
    default_priority: u32,
    pool_shape: String,
    bucket_relations: HashMap<String, Vec<InputSource>>,
    relation_to_bucket: HashMap<String, String>,
    relation_priorities: HashMap<String, u32>,
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
        }
    }
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
                    AttributeValue::Number(n) => "", // Skip numbers as values
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
pub struct Hydrator {
    global_config: BucketConfig,
    buckets: HashMap<String, Bucket>,
}

impl Hydrator {
    pub fn new(config: BucketConfig) -> Self {
        Hydrator {
            global_config: config,
            buckets: HashMap::new(),
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
            eprintln!("Ignoring fact for unknown relation: {}", relation);
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

                    if !is_known {
                        eprintln!("Ignoring fact for unknown relation: {}", relation_name);
                    } else {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ddlog_drain::{AttributeValue, DdlogDrain};

    #[test]
    fn test_relation_specific_path_attributes() {
        // Create facts with different relations using different path attribute names
        let mut fact1 = DdlogFact {
            relation_name: "Relation1".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.2.0".to_string()),
        );
        fact1
            .attributes
            .insert("value".to_string(), AttributeValue::String("A".to_string()));

        let mut fact2 = DdlogFact {
            relation_name: "Relation1".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.1.0".to_string()),
        );
        fact2
            .attributes
            .insert("value".to_string(), AttributeValue::String("B".to_string()));

        let mut fact3 = DdlogFact {
            relation_name: "Relation2".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact3.attributes.insert(
            "custom_path".to_string(),
            AttributeValue::Path("0.3.0".to_string()),
        );
        fact3
            .attributes
            .insert("value".to_string(), AttributeValue::String("C".to_string()));

        let mut fact4 = DdlogFact {
            relation_name: "Relation2".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact4.attributes.insert(
            "custom_path".to_string(),
            AttributeValue::Path("0.2.0".to_string()),
        );
        fact4
            .attributes
            .insert("value".to_string(), AttributeValue::String("D".to_string()));

        // Configure hydrator with relation-specific path attributes
        let config = BucketConfig::new()
            .with_path_attribute("path") // Default path attribute
            .with_relation_path_attribute("Relation2", "custom_path");

        let mut hydrator = Hydrator::new(config);

        // Process facts
        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);
        hydrator.process_fact(fact3);
        hydrator.process_fact(fact4);

        // Check that facts are ordered correctly within each bucket
        let bucket1 = hydrator.buckets().get("Relation1").unwrap();
        let bucket2 = hydrator.buckets().get("Relation2").unwrap();

        // Relation1 uses "path" attribute
        assert_eq!(bucket1.facts[0].path, "0.1.0");
        assert_eq!(bucket1.facts[1].path, "0.2.0");

        // Relation2 uses "custom_path" attribute
        assert_eq!(bucket2.facts[0].path, "0.2.0");
        assert_eq!(bucket2.facts[1].path, "0.3.0");

        // Verify that buckets have the correct path attributes
        assert_eq!(bucket1.path_attribute(), "path");
        assert_eq!(bucket2.path_attribute(), "custom_path");

        // Check the output to ensure the ordering is maintained
        let output = hydrator.dump();

        // Find positions of values in the output
        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();
        let c_pos = output.find("value = C").unwrap();
        let d_pos = output.find("value = D").unwrap();

        // Within Relation1: B (0.1.0) should come before A (0.2.0)
        assert!(b_pos < a_pos);

        // Within Relation2: D (0.2.0) should come before C (0.3.0)
        assert!(d_pos < c_pos);
    }

    #[test]
    fn test_numerical_path_ordering() {
        // Test with paths that should be ordered numerically
        let mut hydrator = Hydrator::new(BucketConfig::new().with_default_path_attribute("path"));

        // Add facts with paths that should be ordered numerically
        let mut fact1 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.10.0".to_string()),
        );
        fact1
            .attributes
            .insert("value".to_string(), AttributeValue::String("B".to_string()));

        let mut fact2 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.2.0".to_string()),
        );
        fact2
            .attributes
            .insert("value".to_string(), AttributeValue::String("A".to_string()));

        let mut fact3 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact3.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.100.0".to_string()),
        );
        fact3
            .attributes
            .insert("value".to_string(), AttributeValue::String("C".to_string()));

        // Process facts in non-numerical order
        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);
        hydrator.process_fact(fact3);

        let bucket = hydrator.buckets().get("TestRelation").unwrap();

        assert_eq!(bucket.facts[0].path, "0.2.0");
        assert_eq!(bucket.facts[1].path, "0.10.0");
        assert_eq!(bucket.facts[2].path, "0.100.0");

        let output = hydrator.dump();
        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();
        let c_pos = output.find("value = C").unwrap();

        assert!(a_pos < b_pos);
        assert!(b_pos < c_pos);
    }

    #[test]
    fn test_basic_hydration() {
        let lines = vec![
            "Relation1{.path = \"0.1.2\", .value = \"data1\"}: +1".to_string(),
            "Relation2{.path = \"0.1\", .value = \"data2\"}: +1".to_string(),
            "Relation1{.path = \"0.1.1\", .value = \"data3\"}: +1".to_string(),
        ];
        let drain = DdlogDrain::new(lines.into_iter());

        let config = BucketConfig::new()
            .with_default_path_attribute("path")
            .with_bucket("Relation1", 10, "path", "value", vec!["Relation1"])
            .with_bucket("Relation2", 5, "path", "value", vec!["Relation2"]);

        let mut hydrator = Hydrator::new(config);
        hydrator.process_drain(drain);

        assert_eq!(hydrator.buckets().len(), 2);

        assert_eq!(hydrator.buckets().get("Relation1").unwrap().facts.len(), 2);
        assert_eq!(hydrator.buckets().get("Relation2").unwrap().facts.len(), 1);

        let output = hydrator.dump();
        assert!(output.contains("Relation1"));
        assert!(output.contains("Relation2"));
        assert!(output.contains("data1"));
        assert!(output.contains("data2"));
        assert!(output.contains("data3"));

        let pool = hydrator.create_pool("test".to_string());
        let pool_output = format!("{}", pool);
        assert!(pool_output.contains("Pool (shape: test)"));
        assert!(pool_output.contains("Bucket: Relation1"));
        assert!(pool_output.contains("Bucket: Relation2"));

        let rel1_pos = pool_output.find("Bucket: Relation1").unwrap();
        let rel2_pos = pool_output.find("Bucket: Relation2").unwrap();
        assert!(rel1_pos < rel2_pos);
    }

    #[test]
    fn test_path_ordering() {
        let lines = vec![
            "TestRelation{.path = \"0.2.1\", .value = \"B\"}: +1".to_string(),
            "TestRelation{.path = \"0.1.2\", .value = \"A\"}: +1".to_string(),
            "TestRelation{.path = \"0.3.0\", .value = \"C\"}: +1".to_string(),
        ];
        let drain = DdlogDrain::new(lines.into_iter());

        let config = BucketConfig::new()
            .with_default_path_attribute("path")
            .with_bucket("TestRelation", 0, "path", "value", vec!["TestRelation"]);

        let mut hydrator = Hydrator::new(config);
        hydrator.process_drain(drain);

        let output = hydrator.dump();

        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();
        let c_pos = output.find("value = C").unwrap();

        assert!(a_pos < b_pos);
        assert!(b_pos < c_pos);
    }

    #[test]
    fn test_custom_path_attribute() {
        let lines = vec![
            "TestRelation{.custom_path = \"0.2\", .value = \"B\"}: +1".to_string(),
            "TestRelation{.custom_path = \"0.1\", .value = \"A\"}: +1".to_string(),
        ];
        let drain = DdlogDrain::new(lines.into_iter());

        let config = BucketConfig::new().with_bucket(
            "TestRelation",
            0,
            "custom_path",
            "value",
            vec!["TestRelation"],
        );

        let mut hydrator = Hydrator::new(config);
        hydrator.process_drain(drain);

        let output = hydrator.dump();

        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();

        assert!(a_pos < b_pos);
    }

    #[test]
    fn test_attribute_value_types() {
        let mut fact1 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.1.2".to_string()),
        );
        fact1.attributes.insert(
            "value".to_string(),
            AttributeValue::String("test".to_string()),
        );

        let mut fact2 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2
            .attributes
            .insert("path".to_string(), AttributeValue::Number(42));
        fact2.attributes.insert(
            "value".to_string(),
            AttributeValue::String("number path".to_string()),
        );

        let mut fact3 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact3.attributes.insert(
            "path".to_string(),
            AttributeValue::String("0.3.4".to_string()),
        );
        fact3.attributes.insert(
            "value".to_string(),
            AttributeValue::String("string path".to_string()),
        );

        let config = BucketConfig::new().with_bucket(
            "TestRelation",
            0,
            "path",
            "value",
            vec!["TestRelation"],
        );
        let mut hydrator = Hydrator::new(config);

        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);
        hydrator.process_fact(fact3);

        let output = hydrator.dump();

        assert!(output.contains("value = test"));
        assert!(output.contains("value = number path"));
        assert!(output.contains("value = string path"));

        let test_pos = output.find("value = test").unwrap();
        let number_pos = output.find("value = number path").unwrap();
        let string_pos = output.find("value = string path").unwrap();

        // "0.1.2" < "42" < "0.3.4" in lexicographical ordering
        assert!(test_pos < number_pos);
        assert!(number_pos < string_pos);
    }

    #[test]
    fn test_pool_creation() {
        // Test with mixed relation types that should be organized by priority
        let lines = vec![
            "TestRelation{.path = \"0.1\", .value = \"test data\"}: +1".to_string(),
            "EmitMermaidLineActivate{.path = \"0.2\", .value = \"activate Counter\"}: +1"
                .to_string(),
            "EmitMermaidLineSignal{.path = \"0.3\", .value = \"signal data\"}: +1".to_string(),
            "OtherRelation{.path = \"0.4\", .value = \"other data\"}: +1".to_string(),
        ];

        let drain = DdlogDrain::new(lines.into_iter());
        let config = BucketConfig::new()
            .with_bucket(
                "EmitMermaidLineActivate",
                10,
                "path",
                "value",
                vec!["EmitMermaidLineActivate"],
            )
            .with_bucket(
                "EmitMermaidLineSignal",
                8,
                "path",
                "value",
                vec!["EmitMermaidLineSignal"],
            )
            .with_bucket("TestRelation", 5, "path", "value", vec!["TestRelation"])
            .with_bucket("OtherRelation", 3, "path", "value", vec!["OtherRelation"]);

        let mut hydrator = Hydrator::new(config);
        hydrator.process_drain(drain);

        let pool = hydrator.create_pool("mermaid".to_string());
        let pool_output = format!("{}", pool);

        assert!(pool_output.contains("Pool (shape: mermaid)"));

        assert!(pool_output.contains("Bucket: TestRelation"));
        assert!(pool_output.contains("Bucket: EmitMermaidLineActivate"));
        assert!(pool_output.contains("Bucket: EmitMermaidLineSignal"));
        assert!(pool_output.contains("Bucket: OtherRelation"));

        assert!(pool_output.contains("test data"));
        assert!(pool_output.contains("activate Counter"));
        assert!(pool_output.contains("signal data"));
        assert!(pool_output.contains("other data"));

        let activate_pos = pool_output.find("Bucket: EmitMermaidLineActivate").unwrap();
        let signal_pos = pool_output.find("Bucket: EmitMermaidLineSignal").unwrap();
        let test_pos = pool_output.find("Bucket: TestRelation").unwrap();
        let other_pos = pool_output.find("Bucket: OtherRelation").unwrap();

        assert!(activate_pos < signal_pos);
        assert!(signal_pos < test_pos);
        assert!(test_pos < other_pos);
    }

    #[test]
    fn test_numerical_path_ordering_primitives() {
        let test_cases = [
            ("0.2.3", "0.10.2"),  // 2 < 10 numerically
            ("1.1.1", "1.2.1"),   // 1 < 2 numerically
            ("1.2.1", "1.10.1"),  // 2 < 10 numerically
            ("0.1.2", "0.1.2.3"), // Shorter path < longer path
            ("0.1.2", "0.1.2"),   // Equal paths
            ("a.1", "a.2"),       // Non-numeric segments compared as strings
            ("1.a", "1.b"),       // Non-numeric segments compared as strings
            ("1.10", "1.a"),      // Numeric segment compared with non-numeric
        ];

        for (path1, path2) in test_cases {
            let result = compare_path_segments(path1, path2);

            match (path1, path2) {
                ("0.2.3", "0.10.2") => assert_eq!(result, Ordering::Less),
                ("1.1.1", "1.2.1") => assert_eq!(result, Ordering::Less),
                ("1.2.1", "1.10.1") => assert_eq!(result, Ordering::Less),
                ("0.1.2", "0.1.2.3") => assert_eq!(result, Ordering::Less),
                ("0.1.2", "0.1.2") => assert_eq!(result, Ordering::Equal),
                ("a.1", "a.2") => assert_eq!(result, Ordering::Less),
                ("1.a", "1.b") => assert_eq!(result, Ordering::Less),
                ("1.10", "1.a") => {
                    // This depends on how we want to handle mixed types
                    // Typically numbers come before letters in ASCII
                    assert!(result == Ordering::Less || result == Ordering::Greater);
                }
                _ => panic!("Unexpected test case"),
            }
        }
    }

    #[test]
    fn test_lexicographical_ordering() {
        let mut hydrator = Hydrator::new(
            BucketConfig::new()
                .with_default_path_attribute("path")
                .with_bucket("TestRelation", 0, "path", "value", vec!["TestRelation"]),
        );

        let mut fact1 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.2.0".to_string()),
        );
        fact1
            .attributes
            .insert("value".to_string(), AttributeValue::String("B".to_string()));

        let mut fact2 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.1.0".to_string()),
        );
        fact2
            .attributes
            .insert("value".to_string(), AttributeValue::String("A".to_string()));

        let mut fact3 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact3.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.3.0".to_string()),
        );
        fact3
            .attributes
            .insert("value".to_string(), AttributeValue::String("C".to_string()));

        // Process facts in non-lexicographical order
        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);
        hydrator.process_fact(fact3);

        let bucket = hydrator.buckets().get("TestRelation").unwrap();

        assert_eq!(bucket.facts[0].path, "0.1.0");
        assert_eq!(bucket.facts[1].path, "0.2.0");
        assert_eq!(bucket.facts[2].path, "0.3.0");

        let output = hydrator.dump();
        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();
        let c_pos = output.find("value = C").unwrap();

        assert!(a_pos < b_pos);
        assert!(b_pos < c_pos);
    }

    #[test]
    fn test_bucket_specific_config() {
        let global_config = BucketConfig::new()
            .with_default_path_attribute("default_path")
            .with_bucket("Relation1", 10, "path1", "value", vec!["Relation1"])
            .with_bucket("Relation2", 20, "path2", "value", vec!["Relation2"]);

        let mut hydrator = Hydrator::new(global_config);

        let mut fact1 = DdlogFact {
            relation_name: "Relation1".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact1
            .attributes
            .insert("path1".to_string(), AttributeValue::Path("1.0".to_string()));

        let mut fact2 = DdlogFact {
            relation_name: "Relation2".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2
            .attributes
            .insert("path2".to_string(), AttributeValue::Path("2.0".to_string()));

        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);

        let bucket1 = hydrator.buckets().get("Relation1").unwrap();
        let bucket2 = hydrator.buckets().get("Relation2").unwrap();

        assert_eq!(bucket1.path_attribute(), "path1");
        assert_eq!(bucket2.path_attribute(), "path2");

        assert_eq!(bucket1.priority(), 10);
        assert_eq!(bucket2.priority(), 20);

        let pool = hydrator.create_pool("custom".to_string());
        let pool_output = format!("{}", pool);

        let rel1_pos = pool_output.find("Bucket: Relation1").unwrap();
        let rel2_pos = pool_output.find("Bucket: Relation2").unwrap();
        assert!(rel2_pos < rel1_pos);
    }

    #[test]
    fn test_pool_shape_config() {
        // Test that the pool shape is correctly set from the BucketConfig
        let config = BucketConfig::new()
            .with_bucket("TestRelation", 10, "path", "value", vec!["TestRelation"])
            .with_pool_shape("custom_shape");

        let mut hydrator = Hydrator::new(config);

        let mut fact = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.1.0".to_string()),
        );
        fact.attributes.insert(
            "value".to_string(),
            AttributeValue::String("test_value".to_string()),
        );

        hydrator.process_fact(fact);

        // Check that the dump uses the configured pool shape
        let output = hydrator.dump();
        assert!(output.contains("Pool (shape: custom_shape)"));
        assert!(output.contains("custom_shape"));

        // Check that we can still override the shape when creating a pool directly
        let pool = hydrator.create_pool("override_shape".to_string());
        let pool_output = format!("{}", pool);
        assert!(pool_output.contains("Pool (shape: override_shape)"));
        assert!(pool_output.contains("override_shape"));
    }

    #[test]
    fn test_ignore_unknown_relations() {
        // Configure hydrator with specific relations
        let config = BucketConfig::new()
            .with_default_path_attribute("path")
            .with_bucket(
                "KnownRelation1",
                10,
                "path",
                "value",
                vec!["KnownRelation1"],
            )
            .with_bucket("KnownRelation2", 5, "path", "value", vec!["KnownRelation2"]);

        let mut hydrator = Hydrator::new(config);

        // Create facts for known and unknown relations
        let mut known_fact1 = DdlogFact {
            relation_name: "KnownRelation1".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        known_fact1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.1.0".to_string()),
        );
        known_fact1.attributes.insert(
            "value".to_string(),
            AttributeValue::String("Known1".to_string()),
        );

        let mut known_fact2 = DdlogFact {
            relation_name: "KnownRelation2".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        known_fact2.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.2.0".to_string()),
        );
        known_fact2.attributes.insert(
            "value".to_string(),
            AttributeValue::String("Known2".to_string()),
        );

        let mut unknown_fact = DdlogFact {
            relation_name: "UnknownRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        unknown_fact.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.3.0".to_string()),
        );
        unknown_fact.attributes.insert(
            "value".to_string(),
            AttributeValue::String("Unknown".to_string()),
        );

        // Process all facts
        hydrator.process_fact(known_fact1);
        hydrator.process_fact(known_fact2);
        hydrator.process_fact(unknown_fact);

        // Verify that only known relations were processed
        assert_eq!(hydrator.buckets().len(), 2);
        assert!(hydrator.buckets().contains_key("KnownRelation1"));
        assert!(hydrator.buckets().contains_key("KnownRelation2"));
        assert!(!hydrator.buckets().contains_key("UnknownRelation"));

        // Verify the output doesn't contain the unknown relation
        let output = hydrator.dump();
        assert!(output.contains("Known1"));
        assert!(output.contains("Known2"));
        assert!(!output.contains("Unknown"));
    }

    #[test]
    fn test_multiple_relations_per_bucket() {
        // Configure hydrator with multiple relations mapped to the same bucket
        let config = BucketConfig::new().with_bucket(
            "MermaidBucket",
            10,
            "path",
            "value",
            vec![
                "EmitMermaidLineActivate",
                "EmitMermaidLineSignal",
                "EmitMermaidLineDeactivate",
            ],
        );

        let mut hydrator = Hydrator::new(config);

        // Create facts for different relations that should go into the same bucket
        let mut fact1 = DdlogFact {
            relation_name: "EmitMermaidLineActivate".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact1.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.1.0".to_string()),
        );
        fact1.attributes.insert(
            "value".to_string(),
            AttributeValue::String("activate Component".to_string()),
        );

        let mut fact2 = DdlogFact {
            relation_name: "EmitMermaidLineSignal".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.2.0".to_string()),
        );
        fact2.attributes.insert(
            "value".to_string(),
            AttributeValue::String("Component -> Other: signal".to_string()),
        );

        let mut fact3 = DdlogFact {
            relation_name: "EmitMermaidLineDeactivate".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact3.attributes.insert(
            "path".to_string(),
            AttributeValue::Path("0.3.0".to_string()),
        );
        fact3.attributes.insert(
            "value".to_string(),
            AttributeValue::String("deactivate Component".to_string()),
        );

        // Process all facts
        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);
        hydrator.process_fact(fact3);

        // Verify that all facts went into a single bucket
        assert_eq!(hydrator.buckets().len(), 1);
        assert!(hydrator.buckets().contains_key("MermaidBucket"));

        let bucket = hydrator.buckets().get("MermaidBucket").unwrap();
        assert_eq!(bucket.facts.len(), 3);

        // Verify that the bucket contains all three relations
        assert_eq!(bucket.relations.len(), 3);
        assert!(bucket
            .relations
            .contains(&"EmitMermaidLineActivate".to_string()));
        assert!(bucket
            .relations
            .contains(&"EmitMermaidLineSignal".to_string()));
        assert!(bucket
            .relations
            .contains(&"EmitMermaidLineDeactivate".to_string()));

        // Verify the output contains all facts in the correct order
        let output = hydrator.dump();
        assert!(output.contains("Bucket: MermaidBucket"));
        assert!(output.contains("Relations: "));
        assert!(output.contains("activate Component"));
        assert!(output.contains("Component -> Other: signal"));
        assert!(output.contains("deactivate Component"));

        // Check ordering by path
        let activate_pos = output.find("activate Component").unwrap();
        let signal_pos = output.find("Component -> Other: signal").unwrap();
        let deactivate_pos = output.find("deactivate Component").unwrap();

        assert!(activate_pos < signal_pos);
        assert!(signal_pos < deactivate_pos);
    }
}
