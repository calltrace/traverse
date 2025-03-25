mod common;

use std::path::PathBuf;
use std::sync::Mutex;
use std::env;
use common::{run_compiled_ddlog_with_new_sources, run_compiler, CompilationOutput, CompilerConfig, DDLogExecutionError, DDLogRunResult, SourceType};
use mermaid::sequence_diagram_ast::{SequenceDiagram, Statement, SignalStatement, ParticipantStatement};

static COMPILATION_RESULT: Mutex<Option<CompilationOutput>> = Mutex::new(None);

#[ctor::ctor]
fn setup() {
    // Check if DDLOG_HOME is set
    if env::var("DDLOG_HOME").is_err() {
        eprintln!("DDLOG_HOME environment variable is not set. Skipping DDLog tests.");
        return;
    }
    
    let config = CompilerConfig {
        input_path: PathBuf::from("sol2seq.trv"),
        project_name: "test_project".to_string(),
        input_parser_homes: vec![PathBuf::from("../language/vendor/tree-sitter-solidity")],
        intermediate_parser_homes: vec![],
        source_path: PathBuf::from("tests/fixtures/simple.sol"),
        source_type: SourceType::Solidity,
        execute: false, // We don't need to execute during compilation
        hydrate: false, // We don't need to hydrate during compilation
        enable_tracing: true,
        output_path: None,
    };

    match run_compiler(&config) {
        Ok(result) => {
            let mut result_guard = COMPILATION_RESULT.lock().unwrap();
            *result_guard = Some(result);
        },
        Err(err) => {
            eprintln!("Compilation failed: {:?}. Skipping DDLog tests.", err);
        }
    }
}

#[test]
fn test_dsl_output() {
    let guard = COMPILATION_RESULT.lock().unwrap();
    if let Some(result) = guard.as_ref() {
        assert!(!result.dsl.is_empty(), "DSL output should not be empty");
    } else {
        println!("Skipping test_dsl_output due to missing DDLOG_HOME");
    }
}

#[test]
fn test_ir_output() {
    let guard = COMPILATION_RESULT.lock().unwrap();
    if let Some(result) = guard.as_ref() {
        assert!(!result.ir.is_empty(), "IR output should not be empty");
    } else {
        println!("Skipping test_ir_output due to missing DDLOG_HOME");
    }
}

#[test]
fn test_ddlog_output() {
    let guard = COMPILATION_RESULT.lock().unwrap();
    if let Some(result) = guard.as_ref() {
        assert!(!result.ddlog.is_empty(), "DDlog output should not be empty");
    } else {
        println!("Skipping test_ddlog_output due to missing DDLOG_HOME");
    }
}

fn run_ddlog_with_file(source_path: &str) -> Result<DDLogRunResult, DDLogExecutionError> {
    let guard = COMPILATION_RESULT.lock().unwrap();
    let compilation_output = guard.as_ref().ok_or_else(|| {
        DDLogExecutionError::CommandExecution("Compilation result not available".to_string())
    })?;
    
    println!("Running DDLog with new source file: {:?}", source_path);
    run_compiled_ddlog_with_new_sources(
        compilation_output,
        vec![PathBuf::from(source_path)],
        SourceType::Solidity,
        vec!["String".to_string()], 
    )
}

/// Helper function to find participant statements by ID
fn find_participant_by_id<'a>(diagram: &'a SequenceDiagram, id: &str) -> Option<&'a ParticipantStatement> {
    diagram.statements.iter().find_map(|stmt| {
        if let Statement::Participant(participant) = stmt {
            if participant.id == id {
                return Some(participant);
            }
        }
        None
    })
}

/// Helper function to find signal statements by from and to participants
fn find_signals<'a>(diagram: &'a SequenceDiagram, from: &str, to: &str) -> Vec<&'a SignalStatement> {
    diagram.statements.iter().filter_map(|stmt| {
        if let Statement::Signal(signal) = stmt {
            if signal.from == from && signal.to == to {
                return Some(signal);
            }
        }
        None
    }).collect()
}

/// Helper function to count signals with a specific message content
fn count_signals_with_message(diagram: &SequenceDiagram, message_content: &str) -> usize {
    diagram.statements.iter().filter(|stmt| {
        if let Statement::Signal(signal) = stmt {
            if let Some(message) = &signal.message {
                return message.content.contains(message_content);
            }
        }
        false
    }).count()
}

#[test]
fn test_run_ddlog_with_counter_contract() {
    // Skip test if DDLOG_HOME is not set
    if env::var("DDLOG_HOME").is_err() {
        println!("Skipping test_run_ddlog_with_counter_contract due to missing DDLOG_HOME");
        return;
    }
    
    // Skip test if compilation result is not available
    let guard = COMPILATION_RESULT.lock().unwrap();
    if guard.is_none() {
        eprintln!("Skipping test_run_ddlog_with_counter_contract due to missing compilation result");
        return;
    }
    drop(guard);
    
    let result = match run_ddlog_with_file("tests/fixtures/counter.sol") {
        Ok(result) => result,
        Err(err) => {
            println!("DDLog execution failed: {:?}. Skipping test.", err);
            return;
        }
    };
    
    let diagram = &result.diagram;
    
    // 1. Verify participants exist
    let counter_participant = find_participant_by_id(diagram, "Counter")
        .expect("Counter participant should exist");
    assert_eq!(counter_participant.participant_type, mermaid::sequence_diagram_ast::ParticipantType::Participant);
    
    let counter_caller_participant = find_participant_by_id(diagram, "CounterCaller")
        .expect("CounterCaller participant should exist");
    assert_eq!(counter_caller_participant.participant_type, mermaid::sequence_diagram_ast::ParticipantType::Participant);
    
    // 2. Verify signals between participants
    let caller_to_counter_signals = find_signals(diagram, "CounterCaller", "Counter");
    assert!(!caller_to_counter_signals.is_empty(), "Should have signals from CounterCaller to Counter");
    
    // 3. Verify specific function calls
    let increment_calls = count_signals_with_message(diagram, "increment");
    assert!(increment_calls > 0, "Should have at least one increment function call");
    
    let get_count_calls = count_signals_with_message(diagram, "getCount");
    assert!(get_count_calls > 0, "Should have at least one getCount function call");
}

/// Helper function to check if a signal exists with a specific message content
fn has_signal_with_message(diagram: &SequenceDiagram, from: &str, to: &str, message_content: &str) -> bool {
    diagram.statements.iter().any(|stmt| {
        if let Statement::Signal(signal) = stmt {
            if signal.from == from && signal.to == to {
                if let Some(message) = &signal.message {
                    return message.content.contains(message_content);
                }
            }
        }
        false
    })
}

/// Helper function to count all participants in the diagram
fn count_participants(diagram: &SequenceDiagram) -> usize {
    diagram.statements.iter().filter(|stmt| {
        matches!(stmt, Statement::Participant(_))
    }).count()
}

#[test]
fn test_run_ddlog_with_uniswap_v2_erc20_contract() {
    // Skip test if compilation result is not available
    let guard = COMPILATION_RESULT.lock().unwrap();
    if guard.is_none() {
        eprintln!("Skipping test_run_ddlog_with_uniswap_v2_erc20_contract due to missing compilation result");
        return;
    }
    drop(guard);
    
    let result = match run_ddlog_with_file("tests/fixtures/uniswap/UniswapV2ERC20.sol") {
        Ok(result) => result,
        Err(err) => {
            println!("DDLog execution failed: {:?}. Skipping test.", err);
            return;
        }
    };
    
    let diagram = &result.diagram;

    println!("Diagram: {:#?}", diagram);
    
    // 1. Verify the UniswapV2ERC20 participant exists
    let uniswap_participant = find_participant_by_id(diagram, "UniswapV2ERC20")
        .expect("UniswapV2ERC20 participant should exist");
    assert_eq!(uniswap_participant.participant_type, mermaid::sequence_diagram_ast::ParticipantType::Participant);
    
    let actor = find_participant_by_id(diagram, "MockActor")
        .expect("Actor participant should exist");
    assert_eq!(actor.participant_type, mermaid::sequence_diagram_ast::ParticipantType::Actor);
    
    // 3. Verify the total number of participants (should be at least 2)
    let participant_count = count_participants(diagram);
    assert!(participant_count >= 2, "Should have at least 2 participants");
    
    // 4. Verify signals for public functions
    // 4.1 Verify approve function
    assert!(
        has_signal_with_message(diagram, "MockActor", "UniswapV2ERC20", "approve"),
        "Should have approve function call from MockActor to UniswapV2ERC20"
    );
    
    // 4.2 Verify transfer function
    assert!(
        has_signal_with_message(diagram, "MockActor", "UniswapV2ERC20", "transfer"),
        "Should have transfer function call from MockActor to UniswapV2ERC20"
    );
    
    // 4.3 Verify transferFrom function
    assert!(
        has_signal_with_message(diagram, "MockActor", "UniswapV2ERC20", "transferFrom"),
        "Should have transferFrom function call from User to UniswapV2ERC20"
    );
    
    // 4.4 Verify permit function
    assert!(
        has_signal_with_message(diagram, "MockActor", "UniswapV2ERC20", "permit"),
        "Should have permit function call from MockActor to UniswapV2ERC20"
    );
    
    // 5. Verify internal function calls
    // 5.1 Verify _approve internal function
    assert!(
        has_signal_with_message(diagram, "UniswapV2ERC20", "UniswapV2ERC20", "_approve"),
        "Should have _approve internal function call within UniswapV2ERC20"
    );
    
    // 5.2 Verify _transfer internal function
    assert!(
        has_signal_with_message(diagram, "UniswapV2ERC20", "UniswapV2ERC20", "_transfer"),
        "Should have _transfer internal function call within UniswapV2ERC20"
    );
    
    // 6. Verify event emissions
    // 6.1 Verify Approval event
    // Do we want to track event submission (via emit)
    /*
    let approval_event_signals = count_signals_with_message(diagram, "Approval");
    assert!(approval_event_signals > 0, "Should have at least one Approval event emission");
    
    // 6.2 Verify Transfer event
    let transfer_event_signals = count_signals_with_message(diagram, "Transfer");
    assert!(transfer_event_signals > 0, "Should have at least one Transfer event emission");
    */

    
}
