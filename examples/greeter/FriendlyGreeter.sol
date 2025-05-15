// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "./IGreeter.sol";

/**
 * @title Friendly Greeter
 * @dev Implementation of IGreeter that provides friendly greetings
 * @custom:binds-to FriendlyGreeterImpl
 */
contract FriendlyGreeter is IGreeter {
    string private greeting;
    
    constructor() {
        greeting = "Hello, friend!";
    }
    
    /**
     * @notice Returns a friendly greeting message
     * @dev Implementation of IGreeter.greet()
     * @return The friendly greeting message
     */
    function greet() external view override returns (string memory) {
        return greeting;
    }
    
    /**
     * @notice Sets a new friendly greeting message
     * @dev Implementation of IGreeter.setGreeting()
     * @param _greeting The new greeting message
     */
    function setGreeting(string memory _greeting) external override {
        greeting = _greeting;
    }
}
