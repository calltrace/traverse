pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractDeactivateStateTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_deactivate_changes_isActive() public {
        // Test that deactivate modifies isActive
        bool initialValue = contractInstance.isActive();
        contractInstance.deactivate();
        assert(contractInstance.isActive() != initialValue);
    }

}
