pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractDeactivateRevertTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_deactivate_reverts_msg_sender____owner_0() public {
        vm.prank(address(1));
        // Test that deactivate reverts when: msg.sender == owner
        vm.expectRevert(bytes(""));
        contractInstance.deactivate();
        vm.stopPrank();
    }

}
