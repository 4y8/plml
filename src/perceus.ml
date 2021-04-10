open Core.U
open Common

exception Linearity_error

let inter =
  let rec aux acc l = function
      [] -> acc
    | hd :: tl when List.mem hd l -> aux (hd :: acc) l tl
    | _ :: tl -> aux acc l tl
  in aux []

let (--) =
  let rec aux acc l l' =
    match l with
      [] -> acc
    | hd :: tl when List.mem hd l' -> aux acc tl l'
    | hd :: tl -> aux (hd :: acc) tl l'
  in aux []

let inc = List.map ((+) 1)
let dec = List.map (Fun.flip (-) 1)

let fv =
  let rec aux n = function
      Var n' when n' >= n -> [n' - n]
    | Lit _
    | GVar _
    | Clo _
    | Var _ -> []
    | App (e, e') -> aux n e @: aux n e'
    | Lam e -> aux (n + 1) e
    | Dup (_, e)
    | Drop (_, e) -> aux n e
  in aux 0

let opt_dup l e =
  match l with
    [] -> e
  | l -> Dup (l, e)

let rec annlin borrowed owned = function
    Var v when owned = [v] -> Var v
  | Var v when List.mem v borrowed && owned = [] -> Dup([v], Var v)
  | App (e, e') ->
     let o' = inter owned (fv e') in
     let e' = annlin borrowed o' e' in
     let e = annlin (borrowed @ o') (owned -- o') e in
     e $$ e'
  | Lam e when List.mem 0 (fv e) ->
     let ys = fv (Lam e) in
     let e = annlin [] (0 :: inc ys) e in
     opt_dup (ys -- owned) (Clo (ys, e))
  | Lam e ->
     let ys = fv (Lam e) in
     let e = annlin [] (inc ys) e in
     opt_dup (ys -- owned) (Clo (ys, Drop ([0], e)))
  | GVar v -> GVar v
  | Lit l -> Lit l
  | _ -> raise Linearity_error
