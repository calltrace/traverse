pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractSetValueRevertTest0 is Test {
    DemoContract private contractInstance;

    function setUp() public {
        contractInstance = new DemoContract("test", 42);
    }

    function test_setValue_reverts_newValue___0() public {
        vm.expectRevert(bytes("Value must be positive"));
        contractInstance.setValue(0);
    }

}