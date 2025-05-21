// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/**
 * @title MyToken
 * @dev Implementation of the ERC20 token standard that aligns with our Gherkin test examples.
 * This implementation includes functions specifically referenced in our CFG and test examples.
 */
contract MyToken {
    // Token metadata
    string public name;
    string public symbol;
    uint8 public decimals;
    
    // Core token data
    uint256 public totalSupply;
    uint256 private initialSupply;
    mapping(address => uint256) private _balances;
    mapping(address => mapping(address => uint256)) private _allowances;
    
    // Events (as per ERC20 standard)
    event Transfer(address indexed from, address indexed to, uint256 value);
    event Approval(address indexed owner, address indexed spender, uint256 value);
    
    /**
     * @dev Constructor to initialize token details
     * @param _name Token name
     * @param _symbol Token symbol
     * @param _decimals Token decimals (usually 18)
     * @param _initialSupply Initial amount of tokens to mint to the deployer
     */
    constructor(
        string memory _name,
        string memory _symbol,
        uint8 _decimals,
        uint256 _initialSupply
    ) {
        name = _name;
        symbol = _symbol;
        decimals = _decimals;
        initialSupply = _initialSupply;
        
        // Mint initial supply to the contract deployer
        if (_initialSupply > 0) {
            _mint(msg.sender, _initialSupply);
        }
    }
    
    /**
     * @dev Returns the balance of a given account
     * @param account Address to check balance for
     * @return Token balance
     */
    function balanceOf(address account) public view returns (uint256) {
        return _balances[account];
    }
    
    /**
     * @dev Returns the allowance set by owner to spender
     * @param owner Address that approves the allowance
     * @param spender Address that can spend the allowance
     * @return Remaining allowance
     */
    function allowance(address owner, address spender) public view returns (uint256) {
        return _allowances[owner][spender];
    }
    
    /**
     * @dev Transfers token from sender to recipient
     * @param recipient The address receiving tokens
     * @param amount The amount of tokens to transfer
     * @return Success boolean
     */
    function transfer(address recipient, uint256 amount) public returns (bool) {
        require(recipient != address(0), "ERC20: transfer to zero address");
        
        _transfer(msg.sender, recipient, amount);
        return true;
    }
    
    /**
     * @dev Internal function to handle transfers
     * This is specifically mentioned in our CFG example and would be a key function
     * to test in our Gherkin scenarios
     */
    function _transfer(address sender, address recipient, uint256 amount) internal {
        require(sender != address(0), "ERC20: transfer from zero address");
        require(recipient != address(0), "ERC20: transfer to zero address");
        require(amount > 0, "ERC20: transfer amount must be greater than zero");
        require(_balances[sender] >= amount, "ERC20: transfer amount exceeds balance");
        
        // Update balances
        _balances[sender] -= amount;
        _balances[recipient] += amount;
        
        // Emit transfer event
        emit Transfer(sender, recipient, amount);
    }
    
    /**
     * @dev Approves spender to spend tokens on behalf of msg.sender
     * @param spender The address allowed to spend tokens
     * @param amount The amount of tokens allowed to spend
     * @return Success boolean
     */
    function approve(address spender, uint256 amount) public returns (bool) {
        require(spender != address(0), "ERC20: approve to zero address");
        
        _approve(msg.sender, spender, amount);
        return true;
    }
    
    /**
     * @dev Internal function to handle approvals
     */
    function _approve(address owner, address spender, uint256 amount) internal {
        require(owner != address(0), "ERC20: approve from zero address");
        require(spender != address(0), "ERC20: approve to zero address");
        
        _allowances[owner][spender] = amount;
        emit Approval(owner, spender, amount);
    }
    
    /**
     * @dev Transfers tokens from one address to another using allowance mechanism
     * @param sender The address sending tokens (and being debited)
     * @param recipient The address receiving tokens
     * @param amount The amount of tokens to transfer
     * @return Success boolean
     */
    function transferFrom(address sender, address recipient, uint256 amount) public returns (bool) {
        require(sender != address(0), "ERC20: transfer from zero address");
        require(recipient != address(0), "ERC20: transfer to zero address");
        
        // First use allowance
        _spendAllowance(sender, msg.sender, amount);
        
        // Then transfer tokens
        _transfer(sender, recipient, amount);
        return true;
    }
    
    /**
     * @dev Internal function to handle spending allowances
     */
    function _spendAllowance(address owner, address spender, uint256 amount) internal {
        uint256 currentAllowance = allowance(owner, spender);
        require(currentAllowance >= amount, "ERC20: insufficient allowance");
        
        unchecked {
            _approve(owner, spender, currentAllowance - amount);
        }
    }
    
    /**
     * @dev Increases the allowance granted to spender
     * @param spender The address approved to spend tokens
     * @param addedValue The additional amount to approve
     * @return Success boolean
     */
    function increaseAllowance(address spender, uint256 addedValue) public returns (bool) {
        require(spender != address(0), "ERC20: approve to zero address");
        
        _approve(msg.sender, spender, _allowances[msg.sender][spender] + addedValue);
        return true;
    }
    
    /**
     * @dev Decreases the allowance granted to spender
     * @param spender The address approved to spend tokens
     * @param subtractedValue The amount to decrease the allowance by
     * @return Success boolean
     */
    function decreaseAllowance(address spender, uint256 subtractedValue) public returns (bool) {
        require(spender != address(0), "ERC20: approve to zero address");
        
        uint256 currentAllowance = _allowances[msg.sender][spender];
        require(currentAllowance >= subtractedValue, "ERC20: decreased allowance below zero");
        
        unchecked {
            _approve(msg.sender, spender, currentAllowance - subtractedValue);
        }
        
        return true;
    }
    
    /**
     * @dev Internal function to mint new tokens
     * @param account Address receiving the minted tokens
     * @param amount Amount of tokens to mint
     */
    function _mint(address account, uint256 amount) internal {
        require(account != address(0), "ERC20: mint to zero address");
        
        totalSupply += amount;
        _balances[account] += amount;
        emit Transfer(address(0), account, amount);
    }
    
    /**
     * @dev Internal function to burn tokens
     * @param account Address whose tokens will be burned
     * @param amount Amount of tokens to burn
     */
    function _burn(address account, uint256 amount) internal {
        require(account != address(0), "ERC20: burn from zero address");
        require(_balances[account] >= amount, "ERC20: burn amount exceeds balance");
        
        unchecked {
            _balances[account] -= amount;
        }
        totalSupply -= amount;
        
        emit Transfer(account, address(0), amount);
    }
}