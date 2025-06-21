pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractDeployerTest is Test {
    function testDeploy() public {
        string memory arg0 = "test";
        uint256 arg1 = 1;
        DemoContract instance = new DemoContract(arg0, arg1);
        assertTrue(address(instance) != address(0));
    }

}