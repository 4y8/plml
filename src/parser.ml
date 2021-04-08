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

let toplevel =
  (function BLK t :: tl ->
             Some ((match edecl $ t with
                      Some (t, _) -> t
                    | None -> raise Invalid_program), tl)
           | _ -> None)

let parser t =
  match many toplevel t with
    None -> raise Invalid_program
  | Some (p, _) -> p
