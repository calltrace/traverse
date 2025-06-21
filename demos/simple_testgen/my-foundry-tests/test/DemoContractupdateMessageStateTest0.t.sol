pragma solidity ^0.8.0;

import "forge-std/Test.sol";

import "../src/DemoContract.sol";

contract DemoContractUpdateMessageStateChangeTest is Test {
    DemoContract private contractInstance;

    function setUp() public {
        contractInstance = new DemoContract("test", 42);
    }

    function test_updateMessage_changes_message() public {
        string memory initialValue = contractInstance.message();
        contractInstance.updateMessage("updated value");
        assertTrue(keccak256(abi.encodePacked(contractInstance.message())) != keccak256(abi.encodePacked(initialValue)));
    }

}