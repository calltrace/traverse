//! Defines known Solidity built-in functions and properties.

use once_cell::sync::Lazy;
use std::collections::HashMap;

/// Represents a Solidity built-in function or property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinFunction {
    pub name: &'static str,
    /// The type of the object this built-in applies to (e.g., "address", "bytes", "array").
    /// Use "any_array" for array types like uint[], bytes[], etc.
    pub object_type: &'static str,
    /// A string representation of the return type (e.g., "uint256", "bool", "(bool,bytes)", "void").
    pub return_type: &'static str,
    /// Indicates if calling this built-in modifies the state of the object it's called on.
    pub mutates_state: bool,
    // TODO: Add argument types if needed for more complex analysis later.
    // pub arguments: Vec<&'static str>,
}

static BUILTIN_FUNCTIONS: Lazy<HashMap<&'static str, Vec<BuiltinFunction>>> = Lazy::new(|| {
    let mut m = HashMap::new();

    let mut add = |builtin: BuiltinFunction| {
        m.entry(builtin.name).or_insert_with(Vec::new).push(builtin);
    };

    // --- Array Built-ins (Dynamic Storage Arrays T[]) ---
    add(BuiltinFunction {
        name: "push",
        object_type: "any_array",
        // >=0.6.0 returns member reference, <0.6.0 returns void.
        // >=0.8.0 returns nothing (void) again for storage arrays.
        // Let's assume void for simplicity, as the return value is often unused.
        return_type: "void",
        mutates_state: true,
    });
    add(BuiltinFunction {
        name: "pop",
        object_type: "any_array",
        return_type: "void", // Returns void
        mutates_state: true,
    });
    // --- Array/Bytes/String Members ---
    add(BuiltinFunction {
        name: "length",
        object_type: "any_array", // T[], bytes, string
        return_type: "uint256",
        mutates_state: false,
    });
    add(BuiltinFunction {
        name: "length",
        object_type: "bytes", // T[], bytes, string
        return_type: "uint256",
        mutates_state: false,
    });
    add(BuiltinFunction {
        name: "length",
        object_type: "string", // T[], bytes, string
        return_type: "uint256",
        mutates_state: false,
    });

    // --- Address Members ---
    add(BuiltinFunction {
        name: "balance",
        object_type: "address",
        return_type: "uint256",
        mutates_state: false, // Read-only view of balance
    });
    add(BuiltinFunction {
        name: "code",
        object_type: "address",
        return_type: "bytes", // bytes memory
        mutates_state: false,
    });
    add(BuiltinFunction {
        name: "codehash",
        object_type: "address",
        return_type: "bytes32",
        mutates_state: false,
    });
    add(BuiltinFunction {
        name: "transfer", // address payable only before 0.8.0
        object_type: "address",
        return_type: "void",
        mutates_state: true, // Modifies balances, reverts on failure
    });
    add(BuiltinFunction {
        name: "send", // address payable only before 0.8.0
        object_type: "address",
        return_type: "bool", // Returns success status
        mutates_state: true, // Modifies balances
    });
    add(BuiltinFunction {
        name: "call", // address payable before 0.8.0
        object_type: "address",
        return_type: "(bool,bytes)", // (success, return_data)
        mutates_state: true,         // Can modify state of called contract
    });
    add(BuiltinFunction {
        name: "delegatecall", // address payable before 0.8.0
        object_type: "address",
        return_type: "(bool,bytes)", // (success, return_data)
        mutates_state: true,         // Modifies state of *this* contract
    });
    add(BuiltinFunction {
        name: "staticcall", // address payable before 0.8.0
        object_type: "address",
        return_type: "(bool,bytes)", // (success, return_data)
        mutates_state: false,        // Cannot modify state
    });

    // --- Function Type Members ---
    add(BuiltinFunction {
        name: "selector",
        object_type: "function", // External function type
        return_type: "bytes4",
        mutates_state: false,
    });
    add(BuiltinFunction {
        name: "address",
        object_type: "function", // External function type
        return_type: "address",
        mutates_state: false,
    });

    // --- Bytes Members (Specific) ---
    // Note: `bytes` also has `.length` handled above by "any_array" logic.
    // Other operations like slicing `bytes[i]` or concatenation are operators, not members.

    // --- String Members (Specific) ---
    // Note: `string` also has `.length` handled above by "any_array" logic.
    add(BuiltinFunction {
        name: "concat",
        object_type: "string", // Global abi function now, but was member-like? Check docs.
        // Let's assume global `abi.string.concat` for now.
        // Keeping this commented out as it's likely not a direct member.
        return_type: "string", // memory
        mutates_state: false,
    });

    // --- Global Functions/Variables ---
    // These are generally NOT handled by this map, as they aren't accessed via member syntax (`.`).
    // They are typically handled by specific query captures or NodeType checks.
    // Examples:
    // - block.* (timestamp, number, difficulty, gaslimit, basefee, chainid, coinbase) -> NodeType::Evm?
    // - msg.* (sender, value, data, sig) -> NodeType::Evm?
    // - tx.* (origin, gasprice) -> NodeType::Evm?
    // - abi.* (encode, encodePacked, encodeWithSignature, encodeWithSelector, decode) -> NodeType::Abi?
    // - bytes.concat(...) -> Global function?
    // - string.concat(...) -> Global function?
    // - type(T).creationCode / type(T).runtimeCode -> Special `type()` expression
    // - type(T).name / type(T).interfaceId -> Special `type()` expression
    // - type(T).min / type(T).max -> Special `type()` expression
    // - Mathematical: addmod, mulmod -> Global functions
    // - Cryptographic: keccak256, sha256, ripemd160, ecrecover -> Global functions
    // - Contract Related: this, selfdestruct -> Special keywords/nodes
    // - Error Handling: require, assert, revert -> Special nodes (RequireCondition, etc.)

    m
});

/// Checks if a function name corresponds to any known built-in.
pub fn is_builtin(name: &str) -> bool {
    BUILTIN_FUNCTIONS.contains_key(name)
}

/// Looks up a built-in function by its name and the type of the object it's called on.
///
/// Handles generic types like "any_array".
pub fn get_builtin(name: &str, object_type: &str) -> Option<&'static BuiltinFunction> {
    BUILTIN_FUNCTIONS.get(name).and_then(|candidates| {
        candidates.iter().find(|builtin| {
            // Direct match or generic array match
            builtin.object_type == object_type
                || (builtin.object_type == "any_array" && object_type.ends_with("[]"))
                || (builtin.object_type == "any_array" && object_type == "bytes") // bytes has .length like arrays
                || (builtin.object_type == "any_array" && object_type == "string") // string has .length like arrays
                || (builtin.object_type == "bytes" && object_type.ends_with("[]")) // length applies to arrays too
                || (builtin.object_type == "string" && object_type.ends_with("[]"))
            // length applies to arrays too
        })
    })
}

/// Checks if a specific built-in function mutates state.
pub fn is_mutating_builtin(name: &str, object_type: &str) -> bool {
    get_builtin(name, object_type).is_some_and(|b| b.mutates_state)
}

/// Gets the return type string for a specific built-in function.
pub fn get_builtin_return_type(name: &str, object_type: &str) -> Option<&'static str> {
    get_builtin(name, object_type).map(|b| b.return_type)
}
