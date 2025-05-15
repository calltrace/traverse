// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * @title ITokenPair Interface
 * @dev Defines a generic interface for a token pair, inspired by UniswapV2.
 * @custom:binds-to TokenPairKey
 */
interface ITokenPair {
    /**
     * @notice Returns the reserves of token0 and token1.
     * @return reserve0 The reserve of token0.
     * @return reserve1 The reserve of token1.
     */
    function getReserves() external view returns (uint112 reserve0, uint112 reserve1);

    /**
     * @notice Returns the address of token0.
     * @return Address of token0.
     */
    function token0() external view returns (address);

    /**
     * @notice Returns the address of token1.
     * @return Address of token1.
     */
    function token1() external view returns (address);
}

