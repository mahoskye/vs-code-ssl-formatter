; Local variable captures for SSL
; This file defines patterns for identifying local variables and scope

; Procedure parameters
(parameter_declaration: (identifier) @variable.parameter)

; Local variables in DECLARE statements  
(declaration_statement: (identifier) @variable.local)

; Loop counters in FOR statements
(for_block: (identifier) @variable.loop)

; Function arguments
(arguments: (expression (identifier)) @variable.parameter)