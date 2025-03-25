// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract SimpleInteraction {
    uint256 private value;
    
    // This function will be called directly by users
    function setValue(uint256 _newValue) public returns (uint256) {
        value = _newValue;
        // Calling the internal function to double the value
        return doubleValue();
    }
    
    // This function is called by setValue
    function doubleValue() private view returns (uint256) {
        return value * 2;
    }
}