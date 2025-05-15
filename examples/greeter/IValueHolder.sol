// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * @title IValueHolder Interface
 * @dev Defines an interface for getting a uint256 value.
 */
interface IValueHolder {
    /**
     * @notice Returns the stored value.
     * @return The stored uint256 value.
     */
    function getValue() external view returns (uint256);

    /**
     * @notice Returns the amount of tokens an owner allowed to a spender.
     * @param owner The address which owns the funds.
     * @param spender The address which will spend the funds.
     * @return A uint specifying the amount of tokens still available for the spender.
     */
    /// @custom:binds-to ValueHolderKey
    function allowance(address owner, address spender) external view returns (uint);
}

