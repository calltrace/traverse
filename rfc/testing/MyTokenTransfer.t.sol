pragma solidity ^0.8.19;

import "./SimpleERC20.sol";

/**
 * @title TokenTransferTest
 * @dev Test contract for MyToken transfer functionality
 * @notice Auto-generated from BDD test specifications
 */
contract TokenTransferTest is Test {
    // Contract instances
    MyToken internal token;
    
    // Actor addresses (from Gherkin specs)
    address internal owner;
    address internal alice;
    address internal bob;
    address internal charlie;
    
    // Test constants (derived from Gherkin specs)
    string constant TOKEN_NAME = "MyToken";
    string constant TOKEN_SYMBOL = "MTK";
    uint8 constant TOKEN_DECIMALS = 18;
    uint256 constant INITIAL_SUPPLY = 1000000;
    uint256 constant ALICE_INITIAL_BALANCE = 1000;
    
    // CFG path tracking (for advanced coverage analysis)
    event CFGNodeVisited(string nodeId);
    
    // Setup - runs before each test
    function setUp() public {
        // Initialize actor addresses
        owner = address(this);
        alice = makeAddr("alice");
        bob = makeAddr("bob");
        charlie = makeAddr("charlie");
        
        // Fund actors with ETH for gas
        vm.deal(alice, 1 ether);
        vm.deal(bob, 1 ether);
        vm.deal(charlie, 1 ether);
        
        // Deploy token contract
        // @step: "a deployed ERC20 token 'MyToken' with parameters"
        // @cfg_node: MyToken.constructor
        token = new MyToken(TOKEN_NAME, TOKEN_SYMBOL, TOKEN_DECIMALS, INITIAL_SUPPLY);
        
        // @step: "Alice has a balance of 1000 tokens"
        // @cfg_node: MyToken._transfer
        token._transfer(address(this), alice, ALICE_INITIAL_BALANCE);
        
        // Verify initial state matches background assumptions
        // @step: "Bob has a balance of 0 tokens"
        assertEq(token.balanceOf(bob), 0, "Bob should start with 0 tokens");
        assertEq(token.balanceOf(charlie), 0, "Charlie should start with 0 tokens");
        assertEq(token.balanceOf(alice), ALICE_INITIAL_BALANCE, "Alice should have initial tokens");
        
        // Verify contract metadata
        assertEq(token.name(), TOKEN_NAME);
        assertEq(token.symbol(), TOKEN_SYMBOL);
        assertEq(token.decimals(), TOKEN_DECIMALS);
        assertEq(token.totalSupply(), INITIAL_SUPPLY);
    }
    
    /**
     * @dev Verifies the token-supply-constant invariant
     * @notice This is an auto-generated invariant check
     */
    function _checkTotalSupplyInvariant() internal {
        assertEq(
            token.totalSupply(),
            INITIAL_SUPPLY,
            "Invariant violation: total supply must remain constant"
        );
    }
    
    /**
     * @dev Tests basic token transfer between accounts
     * @notice Generated from "Basic token transfer between accounts" scenario
     */
    function testBasicTokenTransfer() public {
        // Expected CFG path: MyToken.transfer -> MyToken._transfer
        
        // @step: "Alice transfers 100 tokens to Bob"
        // @cfg_node: MyToken.transfer
        vm.prank(alice);
        bool success = token.transfer(bob, 100);
        assertTrue(success, "Transfer should succeed");
        
        // Verify @step: "Bob's balance should be 100 tokens"
        assertEq(token.balanceOf(bob), 100, "Bob should have received 100 tokens");
        
        // Verify @step: "Alice's balance should be 900 tokens"
        assertEq(token.balanceOf(alice), 900, "Alice should have 900 tokens left");
        
        // Verify @step: "the total supply should remain 1000000 tokens"
        // @invariant: token-supply-constant
        _checkTotalSupplyInvariant();
    }
    
    /**
     * @dev Tests transfer fails with insufficient balance
     * @notice Generated from "Transfer fails with insufficient balance" scenario
     */
    function testInsufficientBalanceTransfer() public {
        // Expected CFG path: MyToken.transfer -> MyToken._transfer (reverts)
        
        // Record initial balances for later comparison
        uint256 aliceInitialBalance = token.balanceOf(alice);
        uint256 bobInitialBalance = token.balanceOf(bob);
        
        // @step: "Alice attempts to transfer 1500 tokens to Bob"
        // @cfg_node: MyToken.transfer
        vm.prank(alice);
        vm.expectRevert("ERC20: transfer amount exceeds balance");
        token.transfer(bob, 1500);
        
        // Verify @step: "Bob's balance should remain 0 tokens"
        assertEq(
            token.balanceOf(bob),
            bobInitialBalance,
            "Bob's balance should be unchanged"
        );
        
        // Verify @step: "Alice's balance should remain 1000 tokens"
        assertEq(
            token.balanceOf(alice),
            aliceInitialBalance,
            "Alice's balance should be unchanged"
        );
        
        // Verify invariant is preserved even after failed operation
        _checkTotalSupplyInvariant();
    }
    
    /**
     * @dev Tests basic approval functionality (implicit from transferFrom tests)
     * @notice Generated from prerequisite steps
     */
    function testApprove() public {
        // Expected CFG path: MyToken.approve -> MyToken._approve
        
        // @step: Setup for transferFrom tests
        vm.prank(alice);
        bool success = token.approve(bob, 500);
        
        assertTrue(success, "Approval should succeed");
        assertEq(
            token.allowance(alice, bob),
            500,
            "Allowance should be set correctly"
        );
    }
    
    /**
     * @dev Tests transferFrom with approved allowance
     * @notice Generated from "Transfer using transferFrom with approved allowance" scenario
     */
    function testTransferFromWithAllowance() public {
        // Expected CFG path: MyToken.transferFrom -> MyToken._spendAllowance -> MyToken._approve -> MyToken._transfer
        
        // Setup: Alice approves Bob
        vm.prank(alice);
        token.approve(bob, 500);
        assertEq(token.allowance(alice, bob), 500, "Allowance should be set correctly");
        
        // @step: "Bob transfers 300 tokens from Alice to Charlie using transferFrom"
        // @cfg_node: MyToken.transferFrom
        vm.prank(bob);
        bool success = token.transferFrom(alice, charlie, 300);
        assertTrue(success, "TransferFrom should succeed");
        
        // Verify @step: "Charlie's balance should be 300 tokens"
        assertEq(token.balanceOf(charlie), 300, "Charlie should have received 300 tokens");
        
        // Verify @step: "Alice's balance should be 700 tokens"
        assertEq(token.balanceOf(alice), 700, "Alice should have 700 tokens left");
        
        // Verify @step: "Bob's allowance from Alice should be 200 tokens"
        assertEq(token.allowance(alice, bob), 200, "Bob's remaining allowance should be 200");
        
        // Verify invariant
        _checkTotalSupplyInvariant();
    }
    
    /**
     * @dev Tests transferFrom fails with insufficient allowance
     * @notice Generated from "TransferFrom fails with insufficient allowance" scenario
     */
    function testInsufficientAllowanceTransferFrom() public {
        // Expected CFG path: MyToken.transferFrom -> MyToken._spendAllowance (reverts)
        
        // Record initial state
        uint256 aliceInitialBalance = token.balanceOf(alice);
        uint256 charlieInitialBalance = token.balanceOf(charlie);
        
        // Setup: Alice approves Bob for limited amount
        vm.prank(alice);
        token.approve(bob, 100);
        assertEq(token.allowance(alice, bob), 100, "Allowance should be set correctly");
        
        // @step: "Bob attempts to transfer 200 tokens from Alice to Charlie using transferFrom"
        // @cfg_node: MyToken.transferFrom
        vm.prank(bob);
        vm.expectRevert("ERC20: insufficient allowance");
        token.transferFrom(alice, charlie, 200);
        
        // Verify @step: "Charlie's balance should remain 0 tokens"
        assertEq(
            token.balanceOf(charlie),
            charlieInitialBalance,
            "Charlie's balance should be unchanged"
        );
        
        // Verify @step: "Alice's balance should remain 1000 tokens"
        assertEq(
            token.balanceOf(alice),
            aliceInitialBalance,
            "Alice's balance should be unchanged"
        );
        
        // Verify @step: "Bob's allowance from Alice should remain 100 tokens"
        assertEq(token.allowance(alice, bob), 100, "Bob's allowance should be unchanged");
        
        // Verify invariant
        _checkTotalSupplyInvariant();
    }
    
    /**
     * @dev Tests increaseAllowance functionality
     * @notice Generated from extended test coverage requirements
     */
    function testIncreaseAllowance() public {
        // Expected CFG path: MyToken.increaseAllowance -> MyToken._approve
        
        // Setup initial allowance
        vm.prank(alice);
        token.approve(bob, 100);
        assertEq(token.allowance(alice, bob), 100, "Initial allowance should be set");
        
        // Increase allowance
        vm.prank(alice);
        bool success = token.increaseAllowance(bob, 50);
        assertTrue(success, "IncreaseAllowance should succeed");
        
        // Verify new allowance
        assertEq(
            token.allowance(alice, bob),
            150,
            "Allowance should be increased correctly"
        );
    }
    
    /**
     * @dev Tests decreaseAllowance functionality
     * @notice Generated from extended test coverage requirements
     */
    function testDecreaseAllowance() public {
        // Expected CFG path: MyToken.decreaseAllowance -> MyToken._approve
        
        // Setup initial allowance
        vm.prank(alice);
        token.approve(bob, 100);
        assertEq(token.allowance(alice, bob), 100, "Initial allowance should be set");
        
        // Decrease allowance
        vm.prank(alice);
        bool success = token.decreaseAllowance(bob, 40);
        assertTrue(success, "DecreaseAllowance should succeed");
        
        // Verify new allowance
        assertEq(
            token.allowance(alice, bob),
            60,
            "Allowance should be decreased correctly"
        );
    }
    
    /**
     * @dev Tests decreaseAllowance fails when attempting to reduce below zero
     * @notice Generated from extended test coverage requirements
     */
    function testDecreaseAllowanceBelowZero() public {
        // Expected CFG path: MyToken.decreaseAllowance (reverts)
        
        // Setup initial allowance
        vm.prank(alice);
        token.approve(bob, 100);
        assertEq(token.allowance(alice, bob), 100, "Initial allowance should be set");
        
        // Attempt to decrease allowance below zero
        vm.prank(alice);
        vm.expectRevert("ERC20: decreased allowance below zero");
        token.decreaseAllowance(bob, 150);
        
        // Verify allowance unchanged
        assertEq(
            token.allowance(alice, bob),
            100,
            "Allowance should remain unchanged"
        );
    }
    
    /**
     * @dev Tests the transferFrom fully depleting allowance
     * @notice Generated from edge case testing requirements
     */
    function testTransferFromFullAllowance() public {
        // Expected CFG path: MyToken.transferFrom -> MyToken._spendAllowance -> MyToken._approve -> MyToken._transfer
        
        // Setup exact allowance
        vm.prank(alice);
        token.approve(bob, 500);
        
        // Use entire allowance
        vm.prank(bob);
        bool success = token.transferFrom(alice, charlie, 500);
        assertTrue(success, "TransferFrom should succeed");
        
        // Verify allowance is zero
        assertEq(
            token.allowance(alice, bob),
            0,
            "Allowance should be fully depleted"
        );
        
        // Verify token transfer occurred
        assertEq(token.balanceOf(alice), 500, "Alice balance should be reduced");
        assertEq(token.balanceOf(charlie), 500, "Charlie should have received tokens");
        
        // Verify invariant
        _checkTotalSupplyInvariant();
    }
}
