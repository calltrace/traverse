pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/DemoContract.sol";

contract Deployer_DemoContract is Test {
    DemoContract public deployedContract;

    function test_Deploy_DemoContract() public {
        // Deploy DemoContract with constructor parameters
        deployedContract = new DemoContract("test", 1);
        assert(address(deployedContract) != address(0));
    }

}
