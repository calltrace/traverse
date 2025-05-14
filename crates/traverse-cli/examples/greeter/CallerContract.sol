// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "./IGreeter.sol";

/**
 * @title Caller Contract
 * @dev Contract that calls the IGreeter interface without knowing the implementation
 */
contract CallerContract {
    /**
     * @notice The greeter interface reference
     * @dev This will be bound to a specific implementation via the binding file
     * @custom:binds-to FriendlyGreeterImpl
     */
    IGreeter private greeter;
    
    /**
     * @notice Constructor that sets the greeter interface
     * @param _greeter Address of the contract implementing IGreeter
     */
    constructor(address _greeter) {
        greeter = IGreeter(_greeter);
    }
    
    /**
     * @notice Gets a greeting from the greeter
     * @dev Calls the greet() function on the IGreeter implementation
     * @return The greeting message
     */
    function getGreeting() external view returns (string memory) {
        return greeter.greet();
    }
    
    /**
     * @notice Updates the greeting in the greeter
     * @dev Calls the setGreeting() function on the IGreeter implementation
     * @param _newGreeting The new greeting message
     */
    function updateGreeting(string memory _newGreeting) external {
        greeter.setGreeting(_newGreeting);
    }
}
