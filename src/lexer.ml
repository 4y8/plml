open Combo

exception Invalid_program

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
  | TAB of int
  | INDENT
  | DEDENT
  | NL
[@@deriving show]

let keywords = ["class", CLASS; "instance", INSTANCE; "let", LET; "in", IN;
                "where", WHERE]

let opsyms = [":", TYPE; "=>", DARR; "->", SARR; "=", EQU]
let syms = ['\\', LAM; '(', OPAR; ')', CPAR; ',', COMMA]

let indent =
  let isident = function
      '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '_' | '\'' -> true
    | _ -> false
  in
  let id = satisfy isident in
  let id2keyword_opt l =
    let s = inplode l in
    match List.assoc_opt s keywords with
      None -> ID s
    | Some t -> t
  in
  id2keyword_opt <$> (List.cons <$> letter <*> many id)

let op =
  let isop = function
      '<' | '>' | '.' | '&' | '|' | '=' | ':' -> true
      | _ -> false
  in
  let op2sym l =
    let s = inplode l in
    match List.assoc_opt s opsyms with
      None -> OP s
    | Some t -> t
  in op2sym <$> many1 (satisfy isop)

let sym =
  fun s ->
  match s with
    [] -> None
  | hd :: tl ->
     match List.assoc_opt hd syms with
       Some t -> Some (t, tl)
     | None -> None

let newline_tab =
  newline *> ((fun x -> TAB (List.length x)) <$> many tab)

let lex = newline_tab <|> (spaces *> op) <|> (spaces *> indent)
          <|> (spaces *> sym)

let rec get_indent n = function
    [] -> NL :: List.init n (Fun.const DEDENT)
  | TAB n' :: tl when n' > n ->
     NL :: List.init (n' - n) (Fun.const INDENT) @ get_indent n' tl
  | TAB n' :: tl when n' < n ->
     NL :: List.init (n - n') (Fun.const DEDENT) @ get_indent n' tl
  | TAB _ :: tl -> NL :: get_indent n tl
  | t :: tl -> t :: get_indent n tl

let lexer s =
  match many lex (explode s) with
    None -> raise Invalid_program
  | Some (t, _) ->
     get_indent 0 t
