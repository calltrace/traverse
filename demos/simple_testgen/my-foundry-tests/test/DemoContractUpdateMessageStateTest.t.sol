pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract DemoContractUpdateMessageStateTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        // Deploy DemoContract for testing
        contractInstance = new DemoContract("test", 1);
    }

    function test_updateMessage_changes_message() public {
        // Test that updateMessage modifies message
        string memory initialValue = contractInstance.message();
        contractInstance.updateMessage("updated test value");
        assert(keccak256(abi.encodePacked(contractInstance.message())) != keccak256(abi.encodePacked(initialValue)));
    }

}
