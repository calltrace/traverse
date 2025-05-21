sequenceDiagram
    actor User
    participant Token as MyToken
    participant Transfer as _transfer
    participant Approve as _approve
    participant SpendAllowance as _spendAllowance
    participant Mint as _mint
    participant Burn as _burn
    
    User->>Token: constructor(name, symbol, decimals, initialSupply)
    activate Token
    Token->>Mint: _mint(msg.sender, initialSupply)
    activate Mint
    Mint->>Mint: require(account != address(0))
    Mint->>Mint: totalSupply += amount
    Mint->>Mint: _balances[account] += amount
    Mint->>Mint: emit Transfer(address(0), account, amount)
    deactivate Mint
    deactivate Token
    
    User->>Token: transfer(recipient, amount)
    activate Token
    Token->>Token: require(recipient != address(0))
    Token->>Transfer: _transfer(msg.sender, recipient, amount)
    activate Transfer
    Transfer->>Transfer: require(sender != address(0))
    Transfer->>Transfer: require(recipient != address(0))
    Transfer->>Transfer: require(amount > 0)
    Transfer->>Transfer: require(_balances[sender] >= amount)
    Transfer->>Transfer: _balances[sender] -= amount
    Transfer->>Transfer: _balances[recipient] += amount
    Transfer->>Transfer: emit Transfer(sender, recipient, amount)
    deactivate Transfer
    Token-->>User: return true
    deactivate Token
    
    User->>Token: approve(spender, amount)
    activate Token
    Token->>Token: require(spender != address(0))
    Token->>Approve: _approve(msg.sender, spender, amount)
    activate Approve
    Approve->>Approve: require(owner != address(0))
    Approve->>Approve: require(spender != address(0))
    Approve->>Approve: _allowances[owner][spender] = amount
    Approve->>Approve: emit Approval(owner, spender, amount)
    deactivate Approve
    Token-->>User: return true
    deactivate Token
    
    User->>Token: transferFrom(sender, recipient, amount)
    activate Token
    Token->>Token: require(sender != address(0))
    Token->>Token: require(recipient != address(0)) 
    Token->>SpendAllowance: _spendAllowance(sender, msg.sender, amount)
    activate SpendAllowance
    SpendAllowance->>Token: allowance(owner, spender)
    SpendAllowance->>SpendAllowance: require(currentAllowance >= amount)
    SpendAllowance->>Approve: _approve(owner, spender, currentAllowance - amount)
    deactivate SpendAllowance
    Token->>Transfer: _transfer(sender, recipient, amount)
    Token-->>User: return true
    deactivate Token

    User->>Token: balanceOf(account)
    activate Token
    Token-->>User: return _balances[account]
    deactivate Token
    
    User->>Token: allowance(owner, spender)
    activate Token
    Token-->>User: return _allowances[owner][spender]
    deactivate Token