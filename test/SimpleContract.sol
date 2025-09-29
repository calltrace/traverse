// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract SimpleContract {
    uint256 public counter;
    
    event CounterIncremented(uint256 newValue);
    
    function increment() public {
        counter++;
        emit CounterIncremented(counter);
    }
    
    function decrement() public {
        require(counter > 0, "Counter cannot be negative");
        counter--;
    }
    
    function reset() public {
        counter = 0;
    }
}

contract LargeContract {
    function func1() public { func2(); }
    function func2() public { func3(); }
    function func3() public { func4(); }
    function func4() public { func5(); }
    function func5() public { func6(); }
    function func6() public { func7(); }
    function func7() public { func8(); }
    function func8() public { func9(); }
    function func9() public { func10(); }
    function func10() public { func11(); }
    function func11() public { func12(); }
    function func12() public {}
}