pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractSumParametersRevertTest0 is Test {
    DemoContract private contractInstance;

    function setUp() public {
        contractInstance = new DemoContract("test", 42);
    }

    function test_sumParameters_reverts_a___1000____b___1000() public {
        vm.expectRevert(bytes("Inputs too large"));
        contractInstance.sumParameters(342, 1993);
    }

}