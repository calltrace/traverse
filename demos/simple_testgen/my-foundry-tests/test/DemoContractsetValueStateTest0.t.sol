pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractSetValueStateChangeTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        contractInstance = new DemoContract("test", 42);
    }

    function test_setValue_changes_value() public {
        uint256 initialValue = contractInstance.value();
        contractInstance.setValue(100);
        assertTrue(contractInstance.value() != initialValue);
    }

}