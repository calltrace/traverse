pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractSetValueRevertTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_setValue_reverts_newValue___0_0() public {
        // Test that setValue reverts when: _newValue > 0
        vm.expectRevert(bytes("Value must be positive"));
        contractInstance.setValue(42);
    }

    function test_setValue_reverts_msg_sender____owner_1() public {
        vm.prank(address(1));
        // Test that setValue reverts when: msg.sender == owner
        vm.expectRevert(bytes("Only owner can set value"));
        contractInstance.setValue(42);
        vm.stopPrank();
    }

}
