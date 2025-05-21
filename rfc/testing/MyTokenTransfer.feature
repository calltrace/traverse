Feature: ERC20 Token Transfers
  As a token holder
  I want to be able to transfer tokens between accounts
  So that I can make payments and manage my assets securely

  Background:
    Given a deployed ERC20 token "MyToken" with parameters:
      | name         | symbol | decimals | initialSupply |
      | "MyToken"    | "MTK"  | 18       | 1000000       |
    And Alice has a balance of 1000 tokens
    And Bob has a balance of 0 tokens

  Scenario: Basic token transfer between accounts
    When Alice transfers 100 tokens to Bob
    Then Bob's balance should be 100 tokens
    And Alice's balance should be 900 tokens
    And the total supply should remain 1000000 tokens

  Scenario: Transfer fails with insufficient balance
    When Alice attempts to transfer 1500 tokens to Bob
    Then the transaction should revert with message "ERC20: transfer amount exceeds balance"
    And Bob's balance should remain 0 tokens
    And Alice's balance should remain 1000 tokens

  Scenario: Transfer using transferFrom with approved allowance
    Given Alice has approved Bob to spend 500 tokens
    When Bob transfers 300 tokens from Alice to Charlie using transferFrom
    Then Charlie's balance should be 300 tokens
    And Alice's balance should be 700 tokens
    And Bob's allowance from Alice should be 200 tokens

  Scenario: TransferFrom fails with insufficient allowance
    Given Alice has approved Bob to spend 100 tokens
    When Bob attempts to transfer 200 tokens from Alice to Charlie using transferFrom
    Then the transaction should revert with message "ERC20: insufficient allowance"
    And Charlie's balance should remain 0 tokens
    And Alice's balance should remain 1000 tokens
    And Bob's allowance from Alice should remain 100 tokens