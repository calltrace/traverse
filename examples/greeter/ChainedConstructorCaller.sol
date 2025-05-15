
import "./IValueHolder.sol";
// ValueHolder.sol is not directly instantiated here anymore, but its existence and Natspec are crucial for binding.

/**
 * @title ChainedConstructorCaller (Structural Typing with Abstract Address Example)
 * @dev Demonstrates casting an abstract address to an interface and calling a method.
 *      Resolution relies on structural typing via shared Natspec binding keys.
 */
contract ChainedConstructorCaller {

    /**
     * @notice An address that is *not* initialized to a concrete ValueHolder instance.
     * @dev This address is used abstractly. The analysis engine is expected to resolve
     *      calls on IValueHolder(abstractValueHolderAddress) to the concrete ValueHolder
     *      contract based on the shared "@custom:binds-to ValueHolderKey" Natspec tag
     *      present on both IValueHolder.sol and ValueHolder.sol.
     */
    address private abstractValueHolderAddress;

    // Note: We are not passing initialValue anymore as we are not instantiating ValueHolder here.
    // The purpose is to test the call resolution on an abstract address.
    // If a specific ValueHolder instance's state was important, it would need to be deployed
    // and its address provided through other means (e.g., constructor, setter).

    /**
     * @notice Casts an unassigned address to IValueHolder and then calls getValue.
     * @return The value returned by ValueHolder's getValue() method, resolved via IValueHolder.
     * @dev This relies on IValueHolder and ValueHolder sharing a @custom:binds-to key (e.g., ValueHolderKey).
     *      The pattern is InterfaceType(abstractAddress).methodName(args).
     *      The `abstractValueHolderAddress` is intentionally unassigned to demonstrate
     *      that the call graph resolution can work based on type information and bindings
     *      without a concrete instance being new-ed up in this specific call chain.
     */
    function callGetValueOnAbstractAddress() external view returns (uint256) {
        // The `abstractValueHolderAddress` is not explicitly assigned to a `new ValueHolder()` here.
        // Resolution to `ValueHolder.getValue()` depends on `IValueHolder` and `ValueHolder`
        // both being associated with "ValueHolderKey" via their Natspec tags.
        return IValueHolder(abstractValueHolderAddress).getValue();
    }

    /**
     * @notice Another example, assigning the result from a call on an abstract address to a local variable.
     * @return The value returned by ValueHolder's getValue() method, resolved via IValueHolder.
     */
    function callAndAssignFromAbstractAddress() external view returns (uint256) {
        uint256 val = IValueHolder(abstractValueHolderAddress).getValue();
        return val;
    }

    /**
     * @notice Calls the allowance function on an IValueHolder interface,
     *         which is expected to be backed by a public mapping in the concrete implementation.
     * @return The allowance value, resolved via IValueHolder.
     * @dev This demonstrates calling an interface method that corresponds to a public state variable.
     *      The resolution relies on the shared "@custom:binds-to ValueHolderKey".
     */
    function callAllowanceOnAbstractAddress() external view returns (uint) {
        // The `abstractValueHolderAddress` is not explicitly assigned.
        // Resolution to `ValueHolder.allowance()` (the public mapping's getter)
        // depends on `IValueHolder` and `ValueHolder` sharing "ValueHolderKey".
        return IValueHolder(abstractValueHolderAddress).allowance(address(this), address(0x1)); // Using arbitrary addresses for example
    }
}
