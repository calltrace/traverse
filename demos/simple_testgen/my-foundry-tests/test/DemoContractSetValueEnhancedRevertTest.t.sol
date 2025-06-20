pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractSetValueEnhancedRevertTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_setValue_reverts_newValue___0_0() public {
        // Test that setValue reverts when: _newValue > 0
        // Invariant breaker found 10 counterexample(s)
        // Counterexample: _newValue = 0
        vm.expectRevert(bytes("Value must be positive"));
        contractInstance.setValue(0);
    }

}
