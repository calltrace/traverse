pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractSetValueAccessControlTest is Test {
    DemoContract private contractInstance;
    address private owner = address(this);

    function setUp() public {
        // Deploy DemoContract for access control testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_setValue_reverts_onlyOwner_1() public {
        // Test that setValue reverts when called by unauthorized address
        // Access control condition: msg.sender == owner
        vm.prank(address(0x1));
        vm.expectRevert(bytes("Only owner can set value"));
        contractInstance.setValue(42);
        vm.stopPrank();
    }

}
