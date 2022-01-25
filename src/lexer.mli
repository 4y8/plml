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
  | EQU
  | COMMA
  | INT of int
  | TAB of int
  | KTAB of int
  | INDENT
  | DEDENT
  | NL
[@@deriving show]

val lexer : string -> t list

exception Invalid_program
