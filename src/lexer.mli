type t
  = LAM
  | CLASS
  | INSTANCE
  | LET
  | IN
  | WHERE
  | OPAR
  | CPAR
  | ID of string
  | OP of string
  | SARR
  | DARR
  | TYPE

val lexer : (char, t) Combo.parser
