open Core

type t
  = Clo of t list * t
  | Env of int
  | Arg
  | GVar of string
  | App of t * t
  | Dup of t list * t
  | Drop of t list * t
  | Lit of Syntax.lit
[@@deriving show]

let rec closure_convert env =
  let convert_list = List.map (fun v -> closure_convert env (U.Var v)) in
  function
    U.Var 0 -> Arg
  | U.Var n -> Env (Common.index n env + 1)
  | U.App (e, e') -> App (closure_convert env e, closure_convert env e')
  | U.Lit c -> Lit c
  | U.GVar v -> GVar v
  | U.Dup (l, e) -> Dup (convert_list l, closure_convert env e)
  | U.Drop (l, e) -> Drop (convert_list l, closure_convert env e)
  | U.Clo (l, e) -> Clo (convert_list l, closure_convert l e)
  | U.Lam _ -> raise Perceus.Linearity_error
