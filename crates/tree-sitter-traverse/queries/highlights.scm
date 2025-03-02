
; Keywords
(rules_block keyword: _ @keyword)
(inference keyword: _ @keyword)
(inference_paths keyword: _ @keyword)
(computation keyword: _ @keyword)
(emit keyword: _ @keyword)
(capture_form keyword: _ @keyword)
(do_form keyword: _ @keyword)
(when_form keyword: _ @keyword)

; Comments
(comment) @comment

; Strings
(string_literal) @string

; Numbers
(num) @number
(number) @number

; Variables
(variable) @variable

; Captures
(capture) @special
(capture_name) @property

; Operators
(predicate_operator) @operator
(prefix_operator) @operator

; Relations and identifiers
(relation) @type
(identifier) @function

; Punctuation
"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"," @punctuation.delimiter
