pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractDeactivateAccessControlTest is Test {
    DemoContract private contractInstance;
    address private owner = address(this);

    function setUp() public {
        // Deploy DemoContract for access control testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_deactivate_reverts_onlyOwner_0() public {
        // Test that deactivate reverts when called by unauthorized address
        // Access control condition: msg.sender == owner
        vm.prank(address(0x1));
        vm.expectRevert(bytes(""));
        contractInstance.deactivate();
        vm.stopPrank();
    }

}
