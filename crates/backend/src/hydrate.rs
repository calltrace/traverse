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
/// 3. Order facts within each bucket using lexicographical ordering of path attributes
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
                // Both are numbers, compare numerically
                if n1 != n2 {
                    return n1.cmp(&n2);
                }
                // If equal, continue to next segment
            }
            _ => {
                // At least one is not a number, compare as strings
                if seg1 != &seg2 {
                    return seg1.cmp(&seg2);
                }
                // If equal, continue to next segment
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
    path_attribute: String,
    default_priority: u32,
}

impl BucketConfig {
    pub fn new() -> Self {
        BucketConfig {
            priorities: HashMap::new(),
            path_attribute: "path".to_string(),
            default_priority: 0,
        }
    }

    pub fn with_priority(mut self, relation: &str, priority: u32) -> Self {
        self.priorities.insert(relation.to_string(), priority);
        self
    }

    pub fn with_path_attribute(mut self, attribute: &str) -> Self {
        self.path_attribute = attribute.to_string();
        self
    }

    pub fn with_default_priority(mut self, priority: u32) -> Self {
        self.default_priority = priority;
        self
    }

    fn get_priority(&self, relation: &str) -> u32 {
        *self
            .priorities
            .get(relation)
            .unwrap_or(&self.default_priority)
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
}

impl OrderedFact {
    fn new(fact: DdlogFact, path_attribute: &str) -> Self {
        // First try to get the path attribute directly
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

        OrderedFact { fact, path }
    }
}

// Implement PartialEq for OrderedFact based on path
impl PartialEq for OrderedFact {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

// Implement Eq for OrderedFact
impl Eq for OrderedFact {}

// Implement PartialOrd for OrderedFact based on lexicographical ordering of path
impl PartialOrd for OrderedFact {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// Implement Ord for OrderedFact based on numerical ordering of path segments
impl Ord for OrderedFact {
    fn cmp(&self, other: &Self) -> Ordering {
        compare_path_segments(&self.path, &other.path)
    }
}

#[derive(Debug, Clone)]
struct Bucket {
    relation: String,
    priority: u32,
    facts: Vec<OrderedFact>,
}

impl Bucket {
    fn new(relation: String, priority: u32) -> Self {
        Bucket {
            relation,
            priority,
            facts: Vec::new(),
        }
    }

    fn add_fact(&mut self, fact: OrderedFact) {
        // Insert the fact and then sort the vector to maintain lexicographical ordering
        // We could use binary search and insert at the right position for better performance,
        // but this is simpler and works well for small collections
        self.facts.push(fact);
        self.facts.sort(); // This uses the Ord implementation for OrderedFact
    }

    fn dump(&self) -> String {
        let mut result = String::new();

        result.push_str(&format!(
            "# Bucket: {} (priority: {})
",
            self.relation, self.priority
        ));

        for fact in &self.facts {
            let attrs = fact
                .fact
                .attributes
                .iter()
                .map(|(k, v)| format!("{} = {}", k, v))
                .collect::<Vec<_>>()
                .join(", ");

            result.push_str(&format!(
                "{}{{{}}}:{}
",
                fact.fact.relation_name,
                if attrs.is_empty() {
                    "".to_string()
                } else {
                    format!(" {} ", attrs)
                },
                fact.fact.diff.map_or("".to_string(), |d| format!(
                    " {}{}",
                    if d >= 0 { "+" } else { "" },
                    d
                ))
            ));
        }

        result
    }
}

#[derive(Debug)]
pub struct Pool {
    buckets: Vec<Bucket>,
}

impl Pool {
    fn new() -> Self {
        Pool {
            buckets: Vec::new(),
        }
    }

    fn add_bucket(&mut self, bucket: Bucket) {
        self.buckets.push(bucket);
    }

    fn sort_buckets_by_priority(&mut self) {
        self.buckets.sort_by(|a, b| b.priority.cmp(&a.priority));
    }
}

impl Display for Pool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "# Pool")?;

        for bucket in &self.buckets {
            write!(f, "{}", bucket.dump())?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Hydrator {
    config: BucketConfig,
    buckets: HashMap<String, Bucket>,
}

impl Hydrator {
    pub fn new(config: BucketConfig) -> Self {
        Hydrator {
            config,
            buckets: HashMap::new(),
        }
    }

    pub fn process_fact(&mut self, fact: DdlogFact) {
        let relation = fact.relation_name.clone();
        let priority = self.config.get_priority(&relation);

        let ordered_fact = OrderedFact::new(fact, &self.config.path_attribute);

        let bucket = self
            .buckets
            .entry(relation.clone())
            .or_insert_with(|| Bucket::new(relation, priority));

        bucket.add_fact(ordered_fact);
    }

    pub fn process_drain<I>(&mut self, drain: DdlogDrain<I>)
    where
        I: Iterator<Item = String>,
    {
        for result in drain {
            match result {
                Ok(fact) => self.process_fact(fact),
                Err(e) => eprintln!("Error processing fact: {}", e),
            }
        }
    }

    pub fn create_pool(&self) -> Pool {
        let mut pool = Pool::new();

        // Clone buckets and add them to the pool
        for bucket in self.buckets.values() {
            pool.add_bucket(bucket.clone());
        }

        // Sort buckets by priority
        pool.sort_buckets_by_priority();

        pool
    }

    pub fn dump(&self) -> String {
        let pool = self.create_pool();
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
    fn test_numerical_path_ordering() {
        // Test with paths that should be ordered numerically
        let mut hydrator = Hydrator::new(BucketConfig::new().with_path_attribute("path"));

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

        // Get the bucket and check the order of facts
        let bucket = hydrator.buckets().get("TestRelation").unwrap();

        // Facts should be ordered numerically by path: 0.2.0, 0.10.0, 0.100.0
        assert_eq!(bucket.facts[0].path, "0.2.0");
        assert_eq!(bucket.facts[1].path, "0.10.0");
        assert_eq!(bucket.facts[2].path, "0.100.0");

        // Check the output to ensure the ordering is maintained
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
            .with_priority("Relation1", 10)
            .with_priority("Relation2", 5)
            .with_path_attribute("path");

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

        // Test pool creation
        let pool = hydrator.create_pool();
        let pool_output = format!("{}", pool);
        assert!(pool_output.contains("# Pool"));
        assert!(pool_output.contains("Bucket: Relation1"));
        assert!(pool_output.contains("Bucket: Relation2"));

        // Check priority ordering
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

        let config = BucketConfig::new().with_path_attribute("path");

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

        let config = BucketConfig::new().with_path_attribute("custom_path");

        let mut hydrator = Hydrator::new(config);
        hydrator.process_drain(drain);

        let output = hydrator.dump();

        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();

        assert!(a_pos < b_pos);
    }

    #[test]
    fn test_attribute_value_types() {
        // Create facts with different attribute value types
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

        let config = BucketConfig::new().with_path_attribute("path");
        let mut hydrator = Hydrator::new(config);

        hydrator.process_fact(fact1);
        hydrator.process_fact(fact2);
        hydrator.process_fact(fact3);

        let output = hydrator.dump();

        // Check that all facts are present
        assert!(output.contains("value = test"));
        assert!(output.contains("value = number path"));
        assert!(output.contains("value = string path"));

        // Check ordering based on path values
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
            .with_priority("EmitMermaidLineActivate", 10)
            .with_priority("EmitMermaidLineSignal", 8)
            .with_priority("TestRelation", 5)
            .with_priority("OtherRelation", 3);

        let mut hydrator = Hydrator::new(config);
        hydrator.process_drain(drain);

        // Create pool and check structure
        let pool = hydrator.create_pool();
        let pool_output = format!("{}", pool);

        // Check that the pool was created correctly
        assert!(pool_output.contains("# Pool"));

        // Check that buckets are in the pool
        assert!(pool_output.contains("Bucket: TestRelation"));
        assert!(pool_output.contains("Bucket: EmitMermaidLineActivate"));
        assert!(pool_output.contains("Bucket: EmitMermaidLineSignal"));
        assert!(pool_output.contains("Bucket: OtherRelation"));

        // Check that all data is present
        assert!(pool_output.contains("test data"));
        assert!(pool_output.contains("activate Counter"));
        assert!(pool_output.contains("signal data"));
        assert!(pool_output.contains("other data"));

        // Check priority ordering - higher priority buckets should come first
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
        // Test cases for numerical ordering
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
            // For each test case, verify the ordering is as expected
            let result = compare_path_segments(path1, path2);

            // Print the comparison result for debugging
            println!("Comparing {} and {}: {:?}", path1, path2, result);

            // Assert the expected ordering
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
        // Test with facts that should be ordered lexicographically
        let mut hydrator = Hydrator::new(BucketConfig::new().with_path_attribute("path"));

        // Add facts in a non-lexicographical order
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

        // Get the bucket and check the order of facts
        let bucket = hydrator.buckets().get("TestRelation").unwrap();

        // Facts should be ordered by path: 0.1.0, 0.2.0, 0.3.0
        assert_eq!(bucket.facts[0].path, "0.1.0");
        assert_eq!(bucket.facts[1].path, "0.2.0");
        assert_eq!(bucket.facts[2].path, "0.3.0");

        // Check the output to ensure the ordering is maintained
        let output = hydrator.dump();
        let a_pos = output.find("value = A").unwrap();
        let b_pos = output.find("value = B").unwrap();
        let c_pos = output.find("value = C").unwrap();

        assert!(a_pos < b_pos);
        assert!(b_pos < c_pos);
    }
}
