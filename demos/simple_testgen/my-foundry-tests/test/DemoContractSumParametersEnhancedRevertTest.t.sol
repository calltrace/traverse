pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractSumParametersEnhancedRevertTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_sumParameters_reverts_a___1000____b___1000_0() public {
        // Test that sumParameters reverts when: a < 1000 && b < 1000
        // Invariant breaker found 10 counterexample(s)
        // Counterexample: a = 2491
        // Counterexample: b = 3045
        vm.expectRevert(bytes("Inputs too large"));
        contractInstance.sumParameters(2491, 3045);
    }

}
