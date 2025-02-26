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
/// - **Pools**: Top-level containers grouping related buckets
/// - **Buckets**: Collections of facts grouped by relation name
/// - **Ordering**: Path-based lexicographical ordering within buckets
/// - **Priorities**: User-defined importance of different relation types
/// - **Hydration**: The process of organizing and assembling facts into coherent output
///
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::fmt::Display;
use regex::Regex;

use crate::ddlog_drain::{AttributeValue, DdlogDrain, DdlogDrainError, DdlogFact};

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

#[derive(Debug, Clone)]
struct Bucket {
    relation: String,
    priority: u32,
    facts: BTreeMap<String, OrderedFact>,
}

impl Bucket {
    fn new(relation: String, priority: u32) -> Self {
        Bucket {
            relation,
            priority,
            facts: BTreeMap::new(),
        }
    }

    fn add_fact(&mut self, fact: OrderedFact) {
        self.facts.insert(fact.path.clone(), fact);
    }

    fn dump(&self) -> String {
        let mut result = String::new();

        result.push_str(&format!(
            "# Bucket: {} (priority: {})
",
            self.relation, self.priority
        ));

        self.facts.iter().for_each(|(_, fact)| {
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
        });

        result
    }
}

/// A Pool is a top-level container for buckets, grouping them by a common attribute
/// derived from their relation names.
#[derive(Debug)]
pub struct Pool {
    name: String,
    buckets: Vec<Bucket>,
}

impl Pool {
    fn new(name: String) -> Self {
        Pool {
            name,
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
        writeln!(f, "# Pool: {}", self.name)?;
        
        for bucket in &self.buckets {
            write!(f, "{}", bucket.dump())?;
        }
        
        Ok(())
    }
}

/// Derives a pool name from a relation name using heuristics.
fn derive_pool_name(relation_name: &str) -> String {
    // For relations starting with "Emit", use "Emit" as the pool name
    if relation_name.starts_with("Emit") {
        return "Emit".to_string();
    }
    
    // For camel case names, use the first part as the pool name
    let re = Regex::new(r"[A-Z][a-z]*").unwrap();
    let parts: Vec<_> = re.find_iter(relation_name).collect();
    if parts.len() > 1 {
        return parts[0].as_str().to_string();
    }
    
    // For simple names, use the name itself
    relation_name.to_string()
}

/// PoolCollection is a container for all pools in the hydration process.
#[derive(Debug)]
pub struct PoolCollection {
    pools: HashMap<String, Pool>,
}

impl PoolCollection {
    fn new() -> Self {
        PoolCollection {
            pools: HashMap::new(),
        }
    }

    fn add_bucket(&mut self, bucket: Bucket) {
        let pool_name = derive_pool_name(&bucket.relation);
        let pool = self.pools
            .entry(pool_name.clone())
            .or_insert_with(|| Pool::new(pool_name));
        
        pool.add_bucket(bucket);
    }

    fn sort_all_pools(&mut self) {
        for pool in self.pools.values_mut() {
            pool.sort_buckets_by_priority();
        }
    }
}

impl Display for PoolCollection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Sort pools by name for consistent output
        let mut pool_names: Vec<_> = self.pools.keys().collect();
        pool_names.sort();
        
        for name in pool_names {
            if let Some(pool) = self.pools.get(name) {
                write!(f, "{}", pool)?;
            }
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

    /// Creates a PoolCollection from the current buckets
    pub fn create_pools(&self) -> PoolCollection {
        let mut pools = PoolCollection::new();
        
        // Clone buckets and add them to pools
        for bucket in self.buckets.values() {
            pools.add_bucket(bucket.clone());
        }
        
        // Sort buckets within each pool
        pools.sort_all_pools();
        
        pools
    }

    /// Legacy method for backward compatibility
    pub fn dump(&self) -> String {
        let pools = self.create_pools();
        format!("{}", pools)
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
        let pools = hydrator.create_pools();
        let pool_output = format!("{}", pools);
        assert!(pool_output.contains("Pool: Relation1"));
        assert!(pool_output.contains("Pool: Relation2"));
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
        fact1.attributes.insert("path".to_string(), AttributeValue::Path("0.1.2".to_string()));
        fact1.attributes.insert("value".to_string(), AttributeValue::String("test".to_string()));

        let mut fact2 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact2.attributes.insert("path".to_string(), AttributeValue::Number(42));
        fact2.attributes.insert("value".to_string(), AttributeValue::String("number path".to_string()));

        let mut fact3 = DdlogFact {
            relation_name: "TestRelation".to_string(),
            attributes: HashMap::new(),
            diff: Some(1),
        };
        fact3.attributes.insert("path".to_string(), AttributeValue::String("0.3.4".to_string()));
        fact3.attributes.insert("value".to_string(), AttributeValue::String("string path".to_string()));

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
        // Test with mixed relation types that should go into different pools
        let lines = vec![
            "TestRelation{.path = \"0.1\", .value = \"test data\"}: +1".to_string(),
            "EmitMermaidLineActivate{.path = \"0.2\", .value = \"activate Counter\"}: +1".to_string(),
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
        
        // Create pools and check structure
        let pools = hydrator.create_pools();
        let pool_output = format!("{}", pools);
        
        // Check that pools were created correctly
        assert!(pool_output.contains("Pool: Test"));
        assert!(pool_output.contains("Pool: Emit"));
        assert!(pool_output.contains("Pool: Other"));
        
        // Check that buckets are in the right pools
        assert!(pool_output.contains("Bucket: TestRelation"));
        assert!(pool_output.contains("Bucket: EmitMermaidLineActivate"));
        assert!(pool_output.contains("Bucket: EmitMermaidLineSignal"));
        assert!(pool_output.contains("Bucket: OtherRelation"));
        
        // Check that all data is present
        assert!(pool_output.contains("test data"));
        assert!(pool_output.contains("activate Counter"));
        assert!(pool_output.contains("signal data"));
        assert!(pool_output.contains("other data"));
    }
}
