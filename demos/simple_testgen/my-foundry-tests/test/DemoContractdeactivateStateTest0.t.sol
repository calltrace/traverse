pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractDeactivateStateChangeTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        contractInstance = new DemoContract("test", 42);
    }

    function test_deactivate_changes_isActive() public {
        bool initialValue = contractInstance.isActive();
        contractInstance.deactivate();
        assertTrue(contractInstance.isActive() != initialValue);
    }

}