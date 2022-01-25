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
  | INT of int
  | TAB of int
  | KTAB of int
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

let int =
  (fun n -> INT (int_of_string (inplode n))) <$> (many1 digit)

let op =
  let isop = function
      '<' | '>' | '.' | '&' | '|' | '=' | ':' | '-' | '+' | '*' | '/' -> true
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
          <|> (spaces *> sym) <|> (spaces *> int)

let rec layout t l =
  match t, l with
    TAB _ :: ((TAB _ :: _) as t), _ -> layout t l
  | TAB n :: tl, m :: _ when m = n ->
     NL :: layout tl l
  | TAB n :: _, m :: ms when n < m ->
     NL :: DEDENT :: layout t ms
  | TAB _ :: tl, _ ->
     layout tl l
  | WHERE :: TAB n :: tl, m :: _ when n > m ->
     WHERE :: INDENT :: layout tl (n :: l)
  | WHERE :: TAB n :: tl, [] when n > 0 ->
     WHERE :: INDENT :: layout tl [n]
  | t :: tl, _ ->
     t :: layout tl l 
  | [], _ :: ms ->
     NL :: DEDENT :: layout [] ms
  | [], [] ->
     []

let lexer s =
  match many lex (explode s) with
    None -> raise Invalid_program
  | Some (t, _) ->
     layout t [0]
