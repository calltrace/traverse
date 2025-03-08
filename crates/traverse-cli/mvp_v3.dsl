(rules  
  /* 
   * Infers a complete function call relationship between contracts.
   * This rule combines information about:
   * - The call expression itself
   * - The caller (contract and function)
   * - The callee contract
   * - The callee function and its return statement
   * 
   * This creates a comprehensive view of a cross-contract function call,
   * capturing both the call and the return flow.
   */
  (infer CallExpressionFunctionContract (?ce_id, ?caller_contract, ?caller_func, ?caller_contract_id, ?caller_func_id, ?callee_func, ?callee_func_id, ?callee_contract_id, ?callee_contract, ?return_stmt_id, ?return_stmt)
    via ((CallExpression ?ce_id, _, _)
        (Caller ?ce_id, ?caller_contract, ?caller_contract_id, ?caller_func, ?caller_func_id)
        (CalleeContract ?ce_id, ?callee_contract_id, ?callee_contract)
        (CalleeFunction ?ce_id, ?callee_func_id, ?callee_func, ?return_stmt_id, ?return_stmt)
    )
  )

  /* 
   * Identifies the caller of a function call.
   * This rule determines:
   * - Which contract contains the function making the call
   * - Which function is making the call
   * 
   * It works by finding the call expression, then traversing up the AST
   * to find the function definition containing the call, and then the
   * contract declaration containing that function.
   */
  (infer Caller (?ce_id, ?contract, ?contract_id, ?func, ?func_id)
    via ((CallExpression ?ce_id, _, _)
        (FunctionDefinition ?func_id, _, _)
        (ContractDeclaration ?contract_id, _, _)
        (Ancestor ?func_id, ?contract_id)
        (Identifier _, ?contract_id, ?contract)
        (Identifier _, ?func_id, ?func)
        (Ancestor ?ce_id, ?func_id)
    )
  )
 
  /* 
   * Identifies the contract being called (the callee contract).
   * This rule determines which contract contains the function being called.
   * 
   * It works by:
   * - Starting with the callee function iformation
   * - Finding the contract declaration that contains that function
   * - Extracting the contract name
   * 
   * This establishes the target contract in a cross-contract function call.
   */
  (infer CalleeContract (?ce_id, ?contract_id, ?contract_name)
    via ((CalleeFunction ?ce_id, ?func_id, _, _, _)
        (FunctionDefinition ?func_id, _, _)
        (ContractDeclaration ?contract_id, _, _)
        (Identifier _, ?contract_id, ?contract_name)
        (Ancestor ?func_id, ?contract_id)
    )
  )

  /*
   * Identifies the caller function in an intra-contract function call.
   * This rule determines:
   * - Which function is making the call
   * - Which contract contains the function
   */
  (infer IntraContractCaller (?ce_id, ?contract, ?contract_id, ?caller_func, ?caller_func_id)
    via ((CallExpression ?ce_id, _, _)
        (FunctionDefinition ?caller_func_id, _, _)
        (Identifier _, ?caller_func_id, ?caller_func)
        (Ancestor ?ce_id, ?caller_func_id)
        (ContractDeclaration ?contract_id, _, _)
        (Identifier _, ?contract_id, ?contract)
        (Ancestor ?caller_func_id, ?contract_id)
    )
  )

  /* 
   * Identifies the callee function in an intra-contract function call.
   * This rule determines:
   * - Which function is being called
   * - Verifies it's in the same contract as the caller
   */
  (infer IntraContractCallee (?ce_id, ?contract_id, ?callee_func, ?callee_func_id)
    via ((CallExpression ?ce_id, _, _)
        (Expression ?expr_id, ?ce_id, _)
        (Identifier ?identifier_id, ?expr_id, ?callee_func)
        (FunctionDefinition ?callee_func_id, _, _)
        (Identifier _, ?callee_func_id, ?callee_func)
        (ContractDeclaration ?contract_id, _, _)
        (Ancestor ?callee_func_id, ?contract_id)
    )
  )

  /* 
   * Identifies the return statement of the callee function in an intra-contract call.
   * This rule finds the return statement within the called function.
   */
  (infer IntraContractReturn (?callee_func_id, ?return_stmt_id, ?return_stmt)
    via ((FunctionDefinition ?callee_func_id, _, _)
        (ReturnStatement ?return_stmt_id, _, ?return_stmt)
        (Ancestor ?return_stmt_id, ?callee_func_id)
    )
  )

  /* 
   * Combines the information from the previous rules to create a complete
   * view of an intra-contract function call.
   */
  (infer IntraContractFunctionCall (?ce_id, ?contract, ?contract_id, ?caller_func, ?caller_func_id, ?callee_func, ?callee_func_id, ?return_stmt_id, ?return_stmt)
    via ((IntraContractCaller ?ce_id, ?contract, ?contract_id, ?caller_func, ?caller_func_id)
        (IntraContractCallee ?ce_id, ?contract_id, ?callee_func, ?callee_func_id)
        (IntraContractReturn ?callee_func_id, ?return_stmt_id, ?return_stmt)
    )
  )



  /* 
   * TODO: if the identifier is not exposed on the LHS, paths will not function properly producing 
   * multiple lines for the same entry.
   * 
   * Identifies the function being called and its return statement.
   * This complex rule determines:
   * - Which function is being called
   * - What that function returns
   * 
   * It works by:
   * 1. Finding the call expression
   * 2. Navigating through member expressions to find the function name
   * 3. Locating the actual function definition with that name
   * 4. Finding the return statement within that function
   * 
   * This captures both the function being called and what it returns,
   * enabling visualization of the complete call-return cycle.
   */
  (infer CalleeFunction (?ce_id, ?func_id, ?func_name, ?return_stmt_id, ?return_stmt)
    via ((CallExpression ?ce_id, _, _)
        (Expression ?expr_id, ?ce_id, _)
        (MemberExpression ?member_expr, ?expr_id, _)
        (Ancestor ?member_expr, ?container_func_id)
        (Identifier ?identifier_id, ?member_expr_id, ?func_name)
        (FunctionDefinition ?func_id, _, _)
        (Identifier _, ?func_id, ?func_name)
        (Ancestor ?identifier_id, ?ce_id)
        (ReturnStatement ?return_stmt_id, _, ?return_stmt)
        (Ancestor ?return_stmt_id, ?func_id)
    )
  )

  /* 
   * Captures the essential elements of a cross-contract function call.
   * This rule extracts and names the key components needed for visualization:
   * - CeId: The unique ID of the call expression
   * - CallerContract: The name of the contract making the call
   * - CallerFunc: The name of the function making the call
   * - CalleeContract: The name of the contract being called
   * - CalleeFunc: The name of the function being called
   * - ReturnStmt: The return statement of the called function
   * 
   * These captured values are used by the emit rules to generate
   * the sequence diagram visualization.
   */
  (capture CallExpressionFunctionContract
    (ce_id @CeId)
    (caller_contract @CallerContract)
    (caller_func @CallerFunc)
    (callee_contract @CalleeContract)
    (callee_func @CalleeFunc)
    (return_stmt @ReturnStmt)
  )

  /* 
   * Emits a Mermaid syntax line that defines the caller contract as a participant.
   * This creates a participant box in the sequence diagram for the contract
   * that initiates the function call.
   * 
   * The @:path:CallerContract ensures that each unique caller contract
   * only appears once in the diagram, regardless of how many calls it makes.
   */
  (emit MermaidLineCallerParticipantLine
    @:path:CallerContract
    (do
      {(format "participant " @CallerContract)}
    )
  )

  /* 
   * Emits a Mermaid syntax line that defines the callee contract as a participant.
   * This creates a participant box in the sequence diagram for the contract
   * that receives the function call.
   * 
   * The @:path:CalleeContract ensures that each unique callee contract
   * only appears once in the diagram, regardless of how many calls it receives.
   */
  (emit MermaidLineCalleeParticipantLine
    @:path:CalleeContract
    (do
      {(format "participant " @CalleeContract)}
    )
  )

  /* 
   * Emits a Mermaid syntax line that represents the function call as an arrow.
   * This creates an arrow in the sequence diagram from the caller contract
   * to the callee contract, labeled with the name of the function being called.
   * 
   * The multiple @:path parameters ensure that each unique function call
   * is represented correctly in the diagram, with the proper source, target,
   * and function name.
   */
  (emit MermaidLineSignalLine
    @:path:CeId
    @:path:CallerContract
    @:path:CalleeContract
    @:path:CalleeFunc
    (do
      {(format @CallerContract "->>" @CalleeContract ": " @CalleeFunc)}
    )
  )

  /* 
   * Emits a Mermaid syntax line that activates the callee contract.
   * This creates an activation bar in the sequence diagram for the callee contract,
   * indicating that it is actively processing the function call.
   * 
   * The @:path parameters ensure that each activation is correctly associated
   * with its specific function call and contract.
   */
  (emit MermaidLineActivateLine
    @:path:CeId
    @:path:CalleeContract
    (do
      {(format "activate " @CalleeContract)}
    )
  )

  /* 
   * Emits a Mermaid syntax line that deactivates the callee contract.
   * This ends the activation bar in the sequence diagram for the callee contract,
   * indicating that it has completed processing the function call.
   * 
   * The @:path parameters ensure that each deactivation is correctly associated
   * with its specific function call and contract.
   */
  (emit MermaidLineDeactivateLine
    @:path:CeId
    @:path:CalleeContract
    (do
      {(format "deactivate " @CalleeContract)}
    )
  )

  /* 
   * Emits a Mermaid syntax line that represents the function return as an arrow.
   * This creates a dashed arrow in the sequence diagram from the callee contract
   * back to the caller contract, labeled with the return statement.
   * 
   * The multiple @:path parameters ensure that each unique function return
   * is represented correctly in the diagram, with the proper source, target,
   * and return value.
   */
  (emit MermaidLineReturnSignalLine
    @:path:CeId
    @:path:CallerContract
    @:path:CalleeContract
    @:path:ReturnStmt
    (do
      {(format @CalleeContract "-->>" @CallerContract ": " @ReturnStmt)}
    )
  )
)
