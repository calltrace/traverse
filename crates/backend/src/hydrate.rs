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
/// 4. Prioritize buckets according to user-defined relation priorities
/// 5. Generate final output by dumping buckets in priority order
///
/// The hydration process ensures that code fragments are assembled in the correct order,
/// respecting dependencies and structural requirements of the generated code.
///
/// ## Key Concepts
///
/// - **Buckets**: Collections of facts grouped by relation name
/// - **Ordering**: Path-based lexicographical ordering within buckets
/// - **Priorities**: User-defined importance of different relation types
/// - **Hydration**: The process of organizing and assembling facts into coherent output
///
use std::collections::{BTreeMap, HashMap};

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
        let path = fact
            .attributes
            .get(path_attribute)
            .and_then(|attr| match attr {
                AttributeValue::Path(p) => Some(p.clone()),
                AttributeValue::String(s) => Some(s.clone()),
                AttributeValue::Number(n) => Some(n.to_string()),
            })
            .unwrap_or_default();

        OrderedFact { fact, path }
    }
}

#[derive(Debug)]
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

    pub fn dump(&self) -> String {
        let mut result = String::new();

        let mut sorted_buckets: Vec<&Bucket> = self.buckets.values().collect();
        sorted_buckets.sort_by(|a, b| b.priority.cmp(&a.priority));

        for bucket in sorted_buckets {
            result.push_str(&bucket.dump());
        }

        result
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
    use crate::ddlog_drain::DdlogDrain;

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

        let rel1_pos = output.find("Relation1").unwrap();
        let rel2_pos = output.find("Relation2").unwrap();
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
}
