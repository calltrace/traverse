// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * @title Greeter Interface
 * @dev Interface for contracts that can provide greeting messages
 * @custom:binds-to FriendlyGreeterImpl
 */
interface IGreeter {
    /**
     * @notice Returns a greeting message
     * @dev Must be implemented by any contract that wants to be a Greeter
     * @return The greeting message as a string
     */
    function greet() external view returns (string memory);
    
    /**
     * @notice Sets a new greeting message
     * @dev Updates the greeting message stored in the contract
     * @param _greeting The new greeting message
     */
    function setGreeting(string memory _greeting) external;
}
