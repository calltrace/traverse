pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractSetValueStateTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_setValue_changes_value() public {
        // Test that setValue modifies value
        uint256 initialValue = contractInstance.value();
        contractInstance.setValue(42);
        assert(contractInstance.value() != initialValue);
    }

}
