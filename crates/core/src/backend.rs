use std::collections::HashMap;

/// Represents a single fact in your Datalog engine.
#[derive(Debug, Clone)]
pub struct Fact {
    pub data: String,
}

/// Represents a structured tree output from the Datalog engine.
#[derive(Debug, PartialEq)]
pub struct Tree {
    pub node: String,
    pub children: Vec<Tree>,
}

/// A `Spec` that holds textual representation of your Datalog program or artifact,
/// plus some metadata (e.g. artifact type).
#[derive(Debug)]
pub struct Spec {
    pub text: String,
    pub artifact_type: String,
}

/// Placeholder for a Tree-sitter AST representation.
#[derive(Debug, Clone)]
pub struct TreeSitterAST {
    description: String,
}

impl TreeSitterAST {
    pub fn new(description: &str) -> Self {
        TreeSitterAST {
            description: description.to_string(),
        }
    }
}

/// The primary trait that any Datalog backend must implement.
pub trait DatalogBackend {
    /// Associated error type for your backend.
    type Error;

    /// Start or initialize the backend.
    fn start(&mut self) -> Result<(), Self::Error>;

    /// Stop or teardown the backend.
    fn stop(&mut self) -> Result<(), Self::Error>;

    /// Submit multiple facts under a given relation name.
    fn submit_input_relation(
        &mut self,
        relation_name: &str,
        facts: &[Fact],
    ) -> Result<(), Self::Error>;

    /// Submit a single fact under a given relation name.
    fn submit_fact(&mut self, relation_name: &str, fact: &Fact) -> Result<(), Self::Error>;

    /// Commit any pending changes or transactions to the logic engine.
    fn commit(&mut self) -> Result<(), Self::Error>;

    /// Collect the output for a given relation and reconstruct it as a structured tree.
    fn collect(&self, relation_name: &str) -> Result<Tree, Self::Error>;
}

/// A separate trait for backends that support “generating” code or resources,
/// with only minimal generative interface (e.g., target folder + final `hydrate`).
pub trait Generative {
    type Error;

    /// Set the target folder where generated artifacts will be stored.
    fn set_target_folder(&mut self, folder: &str) -> &mut Self;

    /// Finalize the generative process (e.g., code generation).
    fn hydrate(&mut self) -> Result<(), Self::Error>;
}

/// Example: A backend struct wrapping DDLog.
/// This struct implements both `Backend` and `Generative`.
/// Note that the fluent "setters" for project name, spec, etc. live on `DDLogBackend` itself.
pub struct DDLogBackend {
    is_running: bool,
    project_name: Option<String>,
    spec: Option<Spec>,
    external_funcs: HashMap<String, TreeSitterAST>,
    target_folder: Option<String>,
}

impl DDLogBackend {
    pub fn new() -> Self {
        Self {
            is_running: false,
            project_name: None,
            spec: None,
            external_funcs: HashMap::new(),
            target_folder: None,
        }
    }

    // -----------------------
    // Fluent "setter" methods
    // -----------------------

    /// Set a project name (not part of `Generative` trait; just a convenience on `DDLogBackend`).
    pub fn set_project_name(&mut self, name: &str) -> &mut Self {
        self.project_name = Some(name.to_string());
        self
    }

    /// Provide a `Spec` containing textual representation and metadata.
    pub fn set_spec(&mut self, spec: Spec) -> &mut Self {
        self.spec = Some(spec);
        self
    }

    /// Add multiple external functions (function name → parse tree).
    pub fn add_external_functions(&mut self, funcs: HashMap<String, TreeSitterAST>) -> &mut Self {
        for (k, v) in funcs {
            self.external_funcs.insert(k, v);
        }
        self
    }

    /// Add or override a single external function.
    pub fn add_external_function(&mut self, name: &str, ast: TreeSitterAST) -> &mut Self {
        self.external_funcs.insert(name.to_string(), ast);
        self
    }
}

impl DatalogBackend for DDLogBackend {
    type Error = String;

    fn start(&mut self) -> Result<(), Self::Error> {
        if self.is_running {
            return Err("DDLogBackend is already running.".into());
        }
        self.is_running = true;
        Ok(())
    }

    fn stop(&mut self) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("DDLogBackend is not running.".into());
        }
        self.is_running = false;
        Ok(())
    }

    fn submit_input_relation(
        &mut self,
        relation_name: &str,
        facts: &[Fact],
    ) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("DDLogBackend is not running.".into());
        }
        println!(
            "DDLogBackend: Submitting {} facts to relation '{}'",
            facts.len(),
            relation_name
        );
        Ok(())
    }

    fn submit_fact(&mut self, relation_name: &str, fact: &Fact) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("DDLogBackend is not running.".into());
        }
        println!(
            "DDLogBackend: Submitting fact {:?} to relation '{}'",
            fact, relation_name
        );
        Ok(())
    }

    fn commit(&mut self) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("DDLogBackend is not running.".into());
        }
        println!("DDLogBackend: Commit called!");
        Ok(())
    }

    fn collect(&self, relation_name: &str) -> Result<Tree, Self::Error> {
        if !self.is_running {
            return Err("DDLogBackend is not running.".into());
        }
        println!("DDLogBackend: Collecting from relation '{}'", relation_name);
        Ok(Tree {
            node: format!("DDLog relation: {}", relation_name),
            children: vec![],
        })
    }
}

/// **Generative** minimal API implementation for `DDLogBackend`.
impl Generative for DDLogBackend {
    type Error = String;

    fn set_target_folder(&mut self, folder: &str) -> &mut Self {
        self.target_folder = Some(folder.to_string());
        self
    }

    fn hydrate(&mut self) -> Result<(), Self::Error> {
        let unnamed_project = "UnnamedProject".to_string();
        let default_target_folder = "./gen".to_string();

        let project_name = self.project_name
            .as_ref()
            .unwrap_or(&unnamed_project);

        let target_folder = self.target_folder
            .as_ref()
            .unwrap_or(&default_target_folder);

        println!("DDLogBackend: Generating artifacts for project '{}' into '{}'.",
                 project_name, target_folder);

        if let Some(ref s) = self.spec {
            println!(
                "  - Using spec artifact_type='{}', text:\n{}",
                s.artifact_type, s.text
            );
        }

        if !self.external_funcs.is_empty() {
            println!("  - External functions:");
            for (func_name, ast) in &self.external_funcs {
                println!("    • {} => AST: {:?}", func_name, ast);
            }
        }

        // ...actual code generation logic goes here...
        Ok(())
    }
}

/// Example: A backend struct for Souffle, which doesn't require generation.
pub struct SouffleBackend {
    is_running: bool,
}

impl SouffleBackend {
    pub fn new() -> Self {
        Self { is_running: false }
    }
}

impl DatalogBackend for SouffleBackend {
    type Error = String;

    fn start(&mut self) -> Result<(), Self::Error> {
        if self.is_running {
            return Err("SouffleBackend is already running.".into());
        }
        self.is_running = true;
        Ok(())
    }

    fn stop(&mut self) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("SouffleBackend is not running.".into());
        }
        self.is_running = false;
        Ok(())
    }

    fn submit_input_relation(
        &mut self,
        relation_name: &str,
        facts: &[Fact],
    ) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("SouffleBackend is not running.".into());
        }
        println!(
            "SouffleBackend: Submitting {} facts to relation '{}'",
            facts.len(),
            relation_name
        );
        Ok(())
    }

    fn submit_fact(&mut self, relation_name: &str, fact: &Fact) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("SouffleBackend is not running.".into());
        }
        println!(
            "SouffleBackend: Submitting fact {:?} to relation '{}'",
            fact, relation_name
        );
        Ok(())
    }

    fn commit(&mut self) -> Result<(), Self::Error> {
        if !self.is_running {
            return Err("SouffleBackend is not running.".into());
        }
        println!("SouffleBackend: Commit called!");
        Ok(())
    }

    fn collect(&self, relation_name: &str) -> Result<Tree, Self::Error> {
        if !self.is_running {
            return Err("SouffleBackend is not running.".into());
        }
        println!("SouffleBackend: Collecting from relation '{}'", relation_name);
        Ok(Tree {
            node: format!("Souffle relation: {}", relation_name),
            children: vec![],
        })
    }
}

/// Unit tests demonstrating usage of these traits.
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ddlog_backend_fluent_api() {
        let mut ddlog = DDLogBackend::new();

        // Prepare a Spec
        let spec = Spec {
            text: "relation R(x) :- S(x).".into(),
            artifact_type: "Datalog".into(),
        };

        // Prepare external function parse trees
        let mut funcs = HashMap::new();
        funcs.insert("my_ext_func".to_string(), TreeSitterAST::new("(function_definition)"));
        funcs.insert("another_func".to_string(), TreeSitterAST::new("(another_definition)"));

        // Configure the backend using fluent setters on DDLogBackend
        ddlog
            .set_project_name("MyDDLogProject")
            .set_spec(spec)
            .add_external_functions(funcs)
            .add_external_function("extra_func", TreeSitterAST::new("(extra_definition)"))
            // The only setter from the Generative trait is the target folder
            .set_target_folder("./custom_gen")
            .hydrate()
            .expect("DDLog hydrate failed");

        // Start, submit facts, commit, collect
        ddlog.start().expect("Failed to start DDLogBackend");
        ddlog
            .submit_fact("S", &Fact { data: "123".into() })
            .expect("Failed to submit fact");
        ddlog.commit().expect("Failed to commit");
        let result_tree = ddlog.collect("R").expect("Failed to collect");
        assert_eq!(
            result_tree,
            Tree {
                node: "DDLog relation: R".into(),
                children: vec![]
            }
        );
        ddlog.stop().expect("Failed to stop DDLogBackend");
    }

    #[test]
    fn test_souffle_backend_basic_flow() {
        let mut souffle = SouffleBackend::new();
        souffle.start().expect("Failed to start SouffleBackend");

        souffle
            .submit_fact("MyRelation", &Fact { data: "example".into() })
            .expect("Submit fact failed");
        souffle.commit().expect("Commit failed");

        let result_tree = souffle.collect("MyRelation").expect("Collect failed");
        assert_eq!(
            result_tree,
            Tree {
                node: "Souffle relation: MyRelation".into(),
                children: vec![]
            }
        );

        souffle.stop().expect("Failed to stop SouffleBackend");
    }
    
    #[test]
    fn test_ddlog_backend_start_stop_errors() {
        let mut ddlog = DDLogBackend::new();

        // Stopping before start should error
        assert!(ddlog.stop().is_err());

        // Starting should succeed
        ddlog.start().expect("Failed to start DDLogBackend");

        // Starting again should fail
        assert!(ddlog.start().is_err());

        // Now stopping should succeed
        ddlog.stop().expect("Failed to stop DDLogBackend");
    }

    #[test]
    fn test_souffle_backend_start_stop_errors() {
        let mut souffle = SouffleBackend::new();

        // Stopping before start should error
        assert!(souffle.stop().is_err());

        // Starting should succeed
        souffle.start().expect("Failed to start SouffleBackend");

        // Starting again should fail
        assert!(souffle.start().is_err());

        // Now stopping should succeed
        souffle.stop().expect("Failed to stop SouffleBackend");
    }
}
