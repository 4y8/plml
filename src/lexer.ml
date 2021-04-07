open Combo

type t
  = LAM
  | CLASS
  | INSTANCE
  | LET
  | IN
  | TYPE
  | WHERE
  | OPAR
  | CPAR
  | ID of string
  | OP of string
  | SARR
  | DARR

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
