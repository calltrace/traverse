// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Counter {
    uint256 private count;
    
    function increment() public returns (uint256) {
        count += 1;
        return count;
    }
    
    function getCount() public view returns (uint256) {
        return count;
    }
}

contract CounterCaller {
    Counter private counter;
    
    constructor(address _counterAddress) {
        counter = Counter(_counterAddress);
    }
    
    function incrementAndGet() public returns (uint256) {
        counter.increment();
        return counter.getCount();
    }
}