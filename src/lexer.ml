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
  | TAB of int
  | BLK of t list
[@@deriving show]

let keywords = ["class", CLASS; "instance", INSTANCE; "let", LET; "in", IN;
                "where", WHERE]

let opsyms = [":", TYPE; "=>", DARR; "->", SARR]
let syms = ['\\', LAM; '(', OPAR; ')', CPAR]

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

let rec get_blk n acc = function
    TAB _ :: ((TAB _ :: _) as l) -> get_blk n acc l
  | TAB n' :: tl when n = n' -> BLK acc, tl
  | (TAB n' :: _) as l when n > n' -> BLK acc, l
  | TAB n' :: tl ->
     let t, tl = get_blk n' [] tl in
     get_blk n (acc @ [t]) tl
  | t :: tl ->
     get_blk n (acc @ [t]) tl
  | [] -> BLK acc, []

let lexer s =
  match many lex (explode s) with
    None -> raise Invalid_program
  | Some (t, _) ->
     let rec lex_blk t =
       match t with
         [] -> []
       | t ->
          let blk, tl = get_blk 0 [] t in
          blk :: lex_blk tl
     in
     lex_blk t
