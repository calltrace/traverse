// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "./ITokenPair.sol";

/**
 * @title Pair Caller Contract
 * @dev Contract that interacts with a token pair via the ITokenPair interface.
 * The actual pair implementation is resolved through binding configuration.
 */
contract PairCaller {
    /**
     * @notice The token pair interface reference.
     * @dev This will be bound to a specific implementation (e.g., ConcretePair)
     * via the binding file, using the key from ITokenPair or this variable.
     * @custom:binds-to TokenPairKey 
     * (Alternatively, if ITokenPair.sol already has the binds-to, this might be redundant
     *  depending on how sol2cg and the resolver prioritize. For clarity, we can include it.)
     */
    ITokenPair private pair;

    /**
     * @notice Constructor that sets the token pair.
     * @param _pairAddress Address of the contract implementing ITokenPair (structurally).
     */
    constructor(address _pairAddress) {
        pair = ITokenPair(_pairAddress);
    }

    /**
     * @notice Gets the reserves from the token pair.
     * @return reserve0 The reserve of token0.
     * @return reserve1 The reserve of token1.
     */
    function fetchReserves() external view returns (uint112 reserve0, uint112 reserve1) {
        return pair.getReserves();
    }

    /**
     * @notice Gets token0 address from the pair.
     * @return Address of token0.
     */
    function getPairToken0() external view returns (address) {
        return pair.token0();
    }
}

