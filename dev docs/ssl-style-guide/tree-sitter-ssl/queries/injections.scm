; SQL injection captures for SSL
; This file defines patterns for highlighting SQL code within strings

; Database parameter patterns within strings
(string (content: (string_content) @sql.keyword))

; SQL keywords within strings (basic pattern)
(string (content: (string_content) @sql.keyword
  (#match? @sql.keyword "(?i)(select|insert|update|delete|from|where|join|order|group|having)")))

; SQL operators within strings
(string (content: (string_content) @sql.operator
  (#match? @sql.operator "(?i)(=|!=|<>|<|>|like|in|between|and|or|not)")))

; SQL function calls within strings
(string (content: (string_content) @sql.function
  (#match? @sql.function "(?i)(count|sum|avg|max|min|now|date|time|count\\(\\))")))