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
  | TAB of int
  | BLK of t list
[@@deriving show]

val lexer : string -> t list
