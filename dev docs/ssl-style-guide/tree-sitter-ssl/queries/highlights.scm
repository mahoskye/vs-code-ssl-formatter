; Highlight captures for SSL

; Keywords (all colon-prefixed)
(( _kw_if ) @keyword)
(( _kw_else ) @keyword)
(( _kw_endif ) @keyword)
(( _kw_while ) @keyword)
(( _kw_endwhile ) @keyword)
(( _kw_for ) @keyword)
(( _kw_next ) @keyword)
(( _kw_to ) @keyword)
(( _kw_step ) @keyword)
(( _kw_begincase ) @keyword)
(( _kw_case ) @keyword)
(( _kw_otherwise ) @keyword)
(( _kw_endcase ) @keyword)
(( _kw_exitcase ) @keyword)
(( _kw_try ) @keyword)
(( _kw_catch ) @keyword)
(( _kw_finally ) @keyword)
(( _kw_endtry ) @keyword)
(( _kw_declare ) @keyword)
(( _kw_default ) @keyword)
(( _kw_parameters ) @keyword)
(( _kw_public ) @keyword)
(( _kw_include ) @keyword)
(( _kw_procedure ) @keyword)
(( _kw_endproc ) @keyword)
(( _kw_return ) @keyword)
(( _kw_class ) @keyword)
(( _kw_inherit ) @keyword)
(( _kw_region ) @keyword)
(( _kw_endregion ) @keyword)
(( _kw_error ) @keyword)
(( _kw_label ) @keyword)
(( _kw_begininlinecode ) @keyword)
(( _kw_endinlinecode ) @keyword)
(( _kw_exitfor ) @keyword)
(( _kw_exitwhile ) @keyword)
(( _kw_loop ) @keyword)
(( _kw_resume ) @keyword)

; Comments / strings / numbers
(comment) @comment
(string) @string
(number) @number
(boolean) @constant.builtin
(nil) @constant.builtin

; Identifiers
(identifier) @variable
(property_access: (identifier) @variable)
(property_access: (property: (identifier) @property))
(method_call: (receiver: (identifier) @variable))
(method_call: (method: (identifier) @function.method))
(call_expression: (function: (identifier) @function))

; Common SSL function names (high-frequency functions)
((call_expression (function) @function.builtin)
  (#match? @function.builtin "(?i)^(SQLEXECUTE|DOPROC|EXECFUNCTION|EMPTY|LEN|USRMES|CHR|AADD|ALLTRIM|AT|NOW|TODAY)$"))

; Operators
(assignment_operator) @operator
(comparison_operator) @operator
(logical_operator) @operator
(additive_operator) @operator
(multiplicative_operator) @operator
(power_operator) @operator

; Blocks
(if_block) @conditional
(while_block) @repeat
(for_block) @repeat
(switch_block) @conditional
(try_catch_block) @exception
(region_block) @namespace
(class_declaration) @type
(procedure_declaration) @function