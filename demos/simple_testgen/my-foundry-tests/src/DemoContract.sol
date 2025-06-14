pragma solidity ^0.8.19;

/**
 * @title DemoContract
 * @dev A minimal contract to demonstrate and test sol2test capabilities.
 */
contract DemoContract {
    uint256 public value;
    address public owner;
    string public message;
    bool public isActive;

    event ValueChanged(address indexed changer, uint256 newValue);
    event MessageUpdated(string newMessage);
    event Activated(bool status);

    constructor(string memory initialMessage, uint256 initialValue) {
        owner = msg.sender;
        message = initialMessage;
        value = initialValue; // Test constructor setting state
        isActive = true;
        emit MessageUpdated(initialMessage);
        emit ValueChanged(msg.sender, initialValue);
        emit Activated(true);
    }

    /**
     * @dev Sets a new value. Must be greater than 0.
     * Emits a ValueChanged event.
     */
    function setValue(uint256 _newValue) public {
        require(_newValue > 0, "Value must be positive"); // For revert test with message
        require(msg.sender == owner, "Only owner can set value"); // Another revert test
        
        value = _newValue;
        emit ValueChanged(msg.sender, _newValue); // For event test (if supported by teststubs)
    }

    /**
     * @dev Updates the message. Message cannot be empty.
     * Emits a MessageUpdated event.
     */
    function updateMessage(string memory _newMessage) public {
        require(bytes(_newMessage).length > 0, "Message cannot be empty"); // For revert test
        
        message = _newMessage;
        emit MessageUpdated(_newMessage);
    }

    /**
     * @dev Deactivates the contract. Can only be called by the owner.
     * This function demonstrates a require without a message.
     */
    function deactivate() public {
        require(msg.sender == owner); // For revert test (will have default panic code)
        isActive = false;
        emit Activated(false);
    }

    /**
     * @dev Returns the current value.
     */
    function getValue() public view returns (uint256) {
        return value;
    }

    /**
     * @dev Returns the current message.
     */
    function getMessage() public view returns (string memory) {
        return message;
    }

    /**
     * @dev Returns the active status.
     */
    function getIsActive() public view returns (bool) {
        return isActive;
    }

    /**
     * @dev A simple function that always reverts with a specific error.
     */
    function alwaysRevertsCustomError() public pure {
        revert("AlwaysFailsError()");
    }

    /**
     * @dev A function that takes multiple parameters and returns their sum.
     * Used to test parameter handling in generated calls.
     */
    function sumParameters(uint256 a, uint256 b) public pure returns (uint256) {
        require(a < 1000 && b < 1000, "Inputs too large"); // Another revert condition
        return a + b;
    }
}

