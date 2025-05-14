// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "./IGreeter.sol";

/**
 * @title Formal Greeter
 * @dev Implementation of IGreeter that provides formal greetings
 * @custom:binds-to FormalGreeterImpl
 */
contract FormalGreeter is IGreeter {
    string private greeting;
    
    constructor() {
        greeting = "Good day, sir/madam.";
    }
    
    /**
     * @notice Returns a formal greeting message
     * @dev Implementation of IGreeter.greet()
     * @return The formal greeting message
     */
    function greet() external view override returns (string memory) {
        return greeting;
    }
    
    /**
     * @notice Sets a new formal greeting message
     * @dev Implementation of IGreeter.setGreeting()
     * @param _greeting The new greeting message
     */
    function setGreeting(string memory _greeting) external override {
        greeting = string(abi.encodePacked("Formally: ", _greeting));
    }
}
