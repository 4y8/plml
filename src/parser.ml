open Combo
open Common
open Lexer
open Syntax

let id = function [] -> None
                | OPAR :: OP v :: CPAR :: tl
                | ID v :: tl -> Some (v, tl)
                | _ -> None

let var = (fun v -> Var v) <$> id

let opls = [["+"; "-"]; ["*"]]

let parse_op =
  let ops = List.map (List.map (fun x -> (fun l r -> App (App (Var x, l), r))
                                         <$ sym (ID x))) opls
  in
  let ops = List.map choice ops in
  List.fold_right chainl1 ops

let rec let_ s =
  (fun v e e' -> Let (v, e, e')) <$ sym LET <*> id <* sym EQU <*> expr <* sym IN
  <*> expr $ s
and lam s =
  (fun v e -> Lam (v, e)) <$ sym LAM <*> id <* sym SARR <*> expr $ s
and par s =
  between (sym OPAR) expr (sym CPAR) $ s
and bloc s =
  sym INDENT *> expr <* sym NL <* sym DEDENT $ s
and expr s =
  parse_op $
    chainl1
      (return (fun e e' -> App (e, e')))
      (let_ <|> var <|> lam <|> par <|> bloc) $ s

let wrap_lam e l =
  let rec aux e = function
      [] -> e
    | hd :: tl -> aux (Lam (hd, e)) tl
  in aux e (List.rev l)

let parse_decl =
  (fun v l e -> v, wrap_lam e l) <$> id <*> many id <* sym EQU <*> expr
  <* sym NL

let edecl =
  (fun (v, e) -> Expr {name = v; expr = e}) <$> parse_decl

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
  let tid =
    (fun v -> TCon (v, [])) <$> id
  in
  let tapp t t' =
    match t with
      TCon (v, l) -> TCon (v, l @ [t'])
    | _ -> raise Invalid_program
  in
  let tfun =
    (fun t t' -> TFun (t, t')) <$ sym SARR
  in
  chainr1 tfun (chainl1 (return tapp) (tvar <|> tid))

let parse_scheme =
  let constr =
    sym OPAR *> sepBy1 (sym COMMA) parse_type <* sym CPAR <* sym DARR
  in
  (fun c t -> Forall (Type.ftv_type [] t, c, t))
  <$> opt [] constr <*> parse_type

let idecl =
  (fun t d -> Instance {ty = t; decls = d})
  <$  sym INSTANCE
  <*> parse_scheme
  <*  sym WHERE <* sym INDENT
  <*> many parse_decl
  <*  sym DEDENT <* sym NL

let parse_tdecl = (fun v t -> v, t) <$> id <* sym TYPE <*> parse_type <* sym NL

let cdecl =
  let getclass t d =
    match t with
      TCon (v, [TVar tv]) ->
       let nms, ts = List.split d in
       let ts = List.map (Type.app_subst_stype [tv, TVar 0]) ts in
       Class {name = v; members = List.combine nms ts}
    | _ -> raise Invalid_program
  in
  getclass
  <$  sym CLASS
  <*> parse_type
  <*  sym WHERE <* sym INDENT
  <*> many parse_tdecl
  <*  sym DEDENT <* sym NL

let toplevel =
  edecl <|> idecl <|> cdecl

let parser t =
  match many toplevel t with
    None -> raise Invalid_program
  | Some (p, _) -> p
