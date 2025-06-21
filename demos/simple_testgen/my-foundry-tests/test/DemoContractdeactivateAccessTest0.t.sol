pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractDeactivateAccessControlTest0 is Test {
    DemoContract private contractInstance;

    function setUp() public {
        contractInstance = new DemoContract("test", 42);
    }

    function test_deactivate_access_control_0() public {
        vm.prank(address(0x1));
        vm.expectRevert();
        contractInstance.deactivate();
        vm.stopPrank();
    }

}