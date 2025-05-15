
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * @title ValueHolder
 * @dev A simple contract that holds a uint value set at construction.
 * It structurally matches IValueHolder and uses the same binding key.
 * @custom:binds-to ValueHolderKey
 */
contract ValueHolder { // Note: Does NOT explicitly state "is IValueHolder"
    uint256 private _value;
    mapping(address => mapping(address => uint256)) public allowance;

    /**
     * @notice Constructor that initializes the value.
     * @param initialValue The initial value to store.
     */
    constructor(uint256 initialValue) {
        _value = initialValue;
    }

    /**
     * @notice Returns the stored value.
     * @return The stored uint256 value.
     */
    function getValue() external view returns (uint256) {
        return _value;
    }
}
