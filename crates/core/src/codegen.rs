use proc_macro2::TokenStream;
use quote::quote;
use std::{io::Write, process::{Command, Stdio}};

// TODO: we need to pass the TS grammar (as sourced from grammar's node-types.json) and the
fn generate_code(project_identifier: &str) -> String {
    let raw_code = generate_code_as_tokens(project_identifier).to_string();
    // rustfmt is not intended to be used programmatically and does not provide a stable API
    let mut rustfmt = Command::new("rustfmt")
        .arg("--emit")
        .arg("stdout")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to spawn rustfmt process");

    if let Some(mut stdin) = rustfmt.stdin.take() {
        use std::io::Write;
        stdin
            .write_all(raw_code.as_bytes())
            .expect("Failed to write to rustfmt stdin");
    }

    let output = rustfmt
        .wait_with_output()
        .expect("Failed to read rustfmt output");

    String::from_utf8_lossy(&output.stdout).to_string()
}

// TODO: we need to pass the TS grammar (as sourced from grammar's node-types.json) and the
fn generate_code_as_tokens(project_identifier: &str) -> proc_macro2::TokenStream {
    let imports = generate_imports(project_identifier);
    let preamble = generate_preamble();
    let body = generate_body(project_identifier);
    let utils = generate_utils();

    quote! {
       #imports

       #preamble

       #body

       #utils
    }
}

fn generate_imports(project_identifier: &str) -> proc_macro2::TokenStream {
    let project = syn::Ident::new(project_identifier, proc_macro2::Span::call_site());

    quote! {
            use std::borrow::Cow;

            // The `Relations` enum enumerates program relations
            use #project::Relations;

            // Type and function definitions generated for each ddlog program
            use #project::typedefs::*;

            // The differential_datalog crate contains the DDlog runtime that is
            // the same for all DDlog programs and simply gets copied to each generated
            // DDlog workspace unmodified (this will change in future releases).

            // The `differential_datalog` crate declares the `HDDlog` type that
            // serves as a reference to a running DDlog program.
            use differential_datalog::api::HDDlog;

            // HDDlog implements several traits:
            use differential_datalog::{DDlog, DDlogDynamic, DDlogInventory};

            // The `differential_datalog::program::config` module declares datatypes
            // used to configure DDlog program on startup.
            use differential_datalog::program::config::{Config, ProfilingConfig};

            // Type that represents a set of changes to DDlog relations.
            // Returned by `DDlog::transaction_commit_dump_changes()`.
            use differential_datalog::DeltaMap;

            // Trait to convert Rust types to/from DDValue.
            // All types used in input and output relations, indexes, and
            // primary keys implement this trait.
            use differential_datalog::ddval::DDValConvert;

            // Generic type that wraps all DDlog values.
            use differential_datalog::ddval::DDValue;

            use differential_datalog::program::RelId; // Numeric relations id.
            use differential_datalog::program::Update; // Type-safe representation of a DDlog command (insert/delete_val/delete_key/...).

            // The `record` module defines dynamically typed representation of DDlog values and commands.
            use differential_datalog::record::Record; // Dynamically typed representation of DDlog values.
            use differential_datalog::record::RelIdentifier; // Relation identifier: either `RelId` or `Cow<str>`.
            use differential_datalog::record::UpdCmd; // Dynamically typed representation of DDlog command.

    }
}

fn generate_preamble() -> proc_macro2::TokenStream {
    quote! {
     // Create a DDlog configuration with 1 worker thread and with the self-profiling feature
     // enabled.
     let config = Config::new()
         .with_timely_workers(1)
         .with_profiling_config(ProfilingConfig::SelfProfiling {
             // Directory to store profiles under or `None` for current directory.
             profile_directory: None
         });
     // Instantiate the DDlog program with this configuration.
     // The second argument of `run_with_config` is a Boolean flag that indicates
     // whether DDlog will track the complete snapshot of output relations.  It
     // should only be set for debugging in order to dump the contents of output
     // tables using `HDDlog::dump_table()`.  Otherwise, indexes are the preferred
     // way to achieve this.
     let (hddlog, init_state) = tutorial_ddlog::run_with_config(config, false)?;

     // Alternatively, use `tutorial_ddlog::run` to instantiate the program with default
     // configuration.  The first argument specifies the number of workers.

     // let (hddlog, init_state) = tutorial_ddlog::run(1, false)?;

     println!("Initial state");
     dump_delta(&hddlog, &init_state);
    }
}

// TODO: facts should be provided as a .dat.  This should only be used as a template.
fn generate_body(project_id: &str) -> proc_macro2::TokenStream {
    quote! {

        /*
         * We perform two transactions that insert in the following two DDlog relations
         * (see `tutorial.dl`):
         *
         * ```
         * input relation Word1(word: string, cat: Category)
         * input relation Word2(word: string, cat: Category)
         * ```
         *
         * The first transactio uses the type-safe API, which should be preferred when
         * writing a client bound to a specific known DDlog program.
         *
         * The second transaction uses the dynamically typed record API.
         */

        // There can be at most one transaction at a time.  Attempt to start another transaction
        // when there is one in execution will return an error.
        hddlog.transaction_start()?;

        // A transaction can consist of multiple `apply_updates()` calls, each taking
        // multiple updates.  An update inserts, deletes or modifies a record in a DDlog
        // relation.
        let updates = vec![
            Update::Insert {
                // We are going to insert..
                relid: Relations::Word1 as RelId, // .. into relation with this Id.
                // `Word1` type, declared in the `types` crate has the same fields as
                // the corresponding DDlog type.
                v: Word1 {
                    word: "foo-".to_string(),
                    cat: Category::CategoryOther,
                }
                .into_ddvalue(),
            },
            Update::Insert {
                relid: Relations::Word2 as RelId,
                v: Word2 {
                    word: "bar".to_string(),
                    cat: Category::CategoryOther,
                }
                .into_ddvalue(),
            },
        ];
        hddlog.apply_updates(&mut updates.into_iter())?;

        // Commit the transaction; returns a `DeltaMap` object that contains the set
        // of changes to output relations produced by the transaction.
        let mut delta = hddlog.transaction_commit_dump_changes()?;
        //assert_eq!(delta, delta_expected);

        println!("\nState after transaction 1");
        dump_delta(&hddlog, &delta);

        // This shows how to extract values from `DeltaMap`.
        println!("\nEnumerating new phrases");

        // Retrieve the set of changes for a particular relation.
        let new_phrases = delta.get_rel(Relations::Phrases as RelId);
        for (val, weight) in new_phrases.iter() {
            // weight = 1 - insert.
            // weight = -1 - delete.
            assert_eq!(*weight, 1);
            let phrase: &Phrases = Phrases::from_ddvalue_ref(val);
            println!("New phrase: {}", phrase.phrase);
        }

        hddlog.transaction_start()?;

        // `Record` type

        let relid_word1 = hddlog.inventory.get_table_id("Word1").unwrap() as RelId;

        // `UpdCmd` is a dynamically typed representation of a DDlog command.
        // It takes a vector or `Record`'s, which represent dynamically typed
        // DDlog values.
        let commands = vec![UpdCmd::Insert(
            RelIdentifier::RelId(relid_word1),
            Record::PosStruct(
                // Positional struct consists of constructor name
                // and a vector of arguments whose number and
                // types must match those of the DDlog constructor.
                // The alternative is `NamedStruct` where arguments
                // are represented as (name, value) pairs.
                Cow::from("Word1"), // Constructor name.
                // Constructor arguments.
                vec![
                    Record::String("buzz".to_string()),
                    Record::PosStruct(Cow::from("CategoryOther"), vec![]),
                ],
            ),
        )];
    }
}

fn generate_epilogue() -> proc_macro2::TokenStream {
    quote! {
        // Use `apply_updates_dynamic` instead of `apply_updates` for dynamically
        // typed commands.
        // This will fail if the records in `commands` don't match the DDlog type
        // declarations (e.g., missing constructor arguments, string instead of integer, etc.)
        hddlog.apply_updates_dynamic(&mut commands.into_iter())?;

        let delta = hddlog.transaction_commit_dump_changes()?;

        println!("\nState after transaction 2");
        dump_delta(&hddlog, &delta);

        ddval_deserialize_test(&hddlog);

        hddlog.stop().unwrap();
        Ok(())
    }
}

fn generate_utils() -> proc_macro2::TokenStream {
    quote! {
        fn dump_delta(ddlog: &HDDlog, delta: &DeltaMap<DDValue>) {
            for (rel, changes) in delta.iter() {
                println!("Changes to relation {}", ddlog.inventory.get_table_name(*rel).unwrap());
                for (val, weight) in changes.iter() {
                    println!("{} {:+}", val, weight);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_imports() {
        let project_identifier = "test_project";
        let imports = generate_imports(project_identifier);
        let expected_imports: TokenStream = quote! {
            use std::borrow::Cow;

            // The `Relations` enum enumerates program relations
            use test_project::Relations;

            // Type and function definitions generated for each ddlog program
            use test_project::typedefs::*;

            // The differential_datalog crate contains the DDlog runtime that is
            // the same for all DDlog programs and simply gets copied to each generated
            // DDlog workspace unmodified (this will change in future releases).

            // The `differential_datalog` crate declares the `HDDlog` type that
            // serves as a reference to a running DDlog program.
            use differential_datalog::api::HDDlog;

            // HDDlog implements several traits:
            use differential_datalog::{DDlog, DDlogDynamic, DDlogInventory};

            // The `differential_datalog::program::config` module declares datatypes
            // used to configure DDlog program on startup.
            use differential_datalog::program::config::{Config, ProfilingConfig};

            // Type that represents a set of changes to DDlog relations.
            // Returned by `DDlog::transaction_commit_dump_changes()`.
            use differential_datalog::DeltaMap;

            // Trait to convert Rust types to/from DDValue.
            // All types used in input and output relations, indexes, and
            // primary keys implement this trait.
            use differential_datalog::ddval::DDValConvert;

            // Generic type that wraps all DDlog values.
            use differential_datalog::ddval::DDValue;

            use differential_datalog::program::RelId; // Numeric relations id.
            use differential_datalog::program::Update; // Type-safe representation of a DDlog command (insert/delete_val/delete_key/...).

            // The `record` module defines dynamically typed representation of DDlog values and commands.
            use differential_datalog::record::Record; // Dynamically typed representation of DDlog values.
            use differential_datalog::record::RelIdentifier; // Relation identifier: either `RelId` or `Cow<str>`.
            use differential_datalog::record::UpdCmd; // Dynamically typed representation of DDlog command.
        };

        assert_eq!(imports.to_string(), expected_imports.to_string());
    }

    /// Test the generate_code_as_tokens` function
    #[test]
    fn test_generate_code_as_tokens() {
        let project_identifier = "test_project";
        let tokens = generate_code_as_tokens(project_identifier);

        assert!(tokens.to_string().contains("use test_project::Relations;"));
        assert!(tokens.to_string().contains("use test_project::typedefs::*;"));
        assert!(tokens.to_string().contains("dump_delta"));
    }

    #[test]
    fn test_generate_code() {
        let project_identifier = "test_project";
        let generated_code = generate_code(project_identifier);

        // Check that the generated code includes expected imports and structure
        assert!(generated_code.contains("use test_project::Relations;"));
        assert!(generated_code.contains("fn dump_delta"));
        assert!(generated_code.contains("fn main"));
    }

    #[test]
    fn test_rustfmt_formatting() {
        let raw_code = r#"
            fn test_function() { let x=42; println!("Unformatted"); }
        "#;

        let mut rustfmt = Command::new("rustfmt")
            .arg("--emit")
            .arg("stdout")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn rustfmt process");

        if let Some(mut stdin) = rustfmt.stdin.take() {
            stdin
                .write_all(raw_code.as_bytes())
                .expect("Failed to write to rustfmt stdin");
        }

        let output = rustfmt
            .wait_with_output()
            .expect("Failed to read rustfmt output");

        let formatted_code = String::from_utf8_lossy(&output.stdout);

        assert!(formatted_code.contains("fn test_function() {"));
        assert!(formatted_code.contains("let x = 42;"));
        assert!(formatted_code.contains("println!(\"Unformatted\");"));
    }

    #[test]
    fn test_full_code_generation_pipeline() {
        let project_identifier = "test_project";
        let raw_code = generate_code(project_identifier);

        assert!(raw_code.contains("use test_project::Relations;"));
        assert!(raw_code.contains("fn main"));
        assert!(raw_code.contains("dump_delta"));

    }

    #[test]
    fn test_generated_code_is_valid_rust_ast() {
        let project_identifier = "test_project";

        // Generate the code as a string
        let generated_code = generate_code(project_identifier);

        // Attempt to parse the generated code as a Rust file
        match syn::parse_file(&generated_code) {
            Ok(_parsed_file) => {
                // If parsing succeeds, the generated code has a valid Rust AST
                println!("The generated code has a valid Rust AST.");
            }
            Err(err) => {
                // If parsing fails, report the error
                panic!("Generated code is invalid Rust: {}", err);
            }
        }
    }
}
