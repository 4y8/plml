open Combo
open Common
open Lexer
open Syntax

let id = function [] -> None
                | ID v :: tl -> Some (v, tl)
                | _ -> None

let var = (fun v -> Var v) <$> id

let rec let_ s =
  (fun v e e' -> Let (v, e, e')) <$ sym LET <*> id <* sym EQU <*> expr <* sym IN
  <*> expr $ s
and lam s =
  (fun v e -> Lam (v, e)) <$ sym LAM <*> id <* sym SARR <*> expr $ s
and par s =
  between (sym OPAR) expr (sym CPAR) $ s
and expr s =
  chainl1 (return (fun e e' -> App (e, e'))) (let_ <|> var <|> lam <|> par) $ s

let wrap_lam e l =
  let rec aux e = function
      [] -> e
    | hd :: tl -> aux (Lam (hd, e)) tl
  in aux e (List.rev l)

let edecl =
  (fun v l e -> Expr {name = v; expr = wrap_lam e l}) <$> id <*> many id
  <* sym EQU <*> expr
(*
let parse_type =
  let tvar =
    let tvarid = function
        ID v :: tl ->
         begin
           match String.get v 0 with
             'a' .. 'z' as c -> Some (c, tl)
           | _ -> None
         end
      | _ -> None
    in
    (fun c ->
      TVar ((Char.code c) - (Char.code 'a'))) <$> tvarid
  in
  let tapp t t' =
    match t with
      TCon (v, l) -> TCon (v, l @ [t'])
    | _ -> raise Invalid_program
  in
  let tfun =
    (fun t t' -> TFun (t, t')) <$ sym SARR
  in
  chainr1 tfun (chainl1 (return tapp) tvar)

let parse_scheme =
  let constr =
    sym OPAR *> sepBy1 (sym COMMA) parse_type <* sym CPAR <* sym DARR
  in
  (fun c t -> Forall (Type.ftv_type [] t, c, t))
  <$> opt [] constr <*> parse_type


let idecl =
  (fun t d -> {ty = t; decls = d}) <$ sym INSTANCE <*> parse_scheme <* sym WHERE
                                                                       *)

let toplevel =
  edecl


let parser t =
  match many toplevel t with
    None -> raise Invalid_program
  | Some (p, _) -> p
