(rules
  /* This is a sample inference rule */
  (infer relation (?x, ?y)
    via (predicate ?x, ?y))
  
  /* This is a sample capture form */
  (capture node_type
    (key1 @variable1)
    (key2 @variable2)
    (when (and @variable1 @variable2))
    (do { expr1 } { expr2 })
  )
  
  /* This is a sample emit form */
  (emit output_node_type
    @variable1
    @variable2
    (when (eq @variable1 "value"))
    (do { expr1 } { expr2 })
  )
)
