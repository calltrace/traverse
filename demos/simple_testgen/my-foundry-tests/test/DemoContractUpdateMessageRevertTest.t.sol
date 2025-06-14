pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractUpdateMessageRevertTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_updateMessage_reverts_bytes__newMessage__length___0_0() public {
        // Test that updateMessage reverts when: bytes(_newMessage).length > 0
        vm.expectRevert(bytes("Message cannot be empty"));
        contractInstance.updateMessage("updated test value");
    }

}
