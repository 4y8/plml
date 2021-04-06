open Syntax

module IR : sig
  type t
    = Var of string
    | Lam of string * stype * t
    | App of t * t
    | Let of string * t * t
    | TLam of int * t
    | TApp of t * stype
    | Proj of int * int * t
    | Dict of t list
    | DLam of stype * t
    | DVar of stype
  [@@deriving show]
end = struct
  type t
    = Var of string
    | Lam of string * stype * t
    | App of t * t
    | Let of string * t * t
    | TLam of int * t
    | TApp of t * stype
    | Proj of int * int * t
    | Dict of t list
    | DLam of stype * t
    | DVar of stype
  [@@deriving show]
end

module F : sig
  type t
    = Var of int
    | GVar of string
    | Lam of stype * t
    | App of t * t
    | TLam of int * t
    | TApp of t * stype
    | Lit of lit
    | Let of t * t
end = struct
  type t
    = Var of int
    | GVar of string
    | Lam of stype * t
    | App of t * t
    | TLam of int * t
    | TApp of t * stype
    | Lit of lit
    | Let of t * t
end

type vtype = V of string | D of stype

let ($$) e e' = F.App (e, e')

let rec purify env = function
    IR.Var v ->
    let i = Common.index (V v) env in
    if i = -1 then F.GVar v else F.Var i
  | IR.App (e, e') -> purify env e $$ purify env e'
  | IR.Lam (v, t, e) -> F.Lam (t, purify (V v :: env) e)
  | IR.TLam (n, e) -> F.TLam (n, purify env e)
  | IR.TApp (e, t) -> F.TApp (purify env e, t)
  | IR.DVar d -> F.Var (Common.index (D d) env)
  | IR.DLam (d, e) -> F.Lam (d, purify (D d :: env) e)
  | IR.Let (v, e, e') -> F.Let (purify env e, purify (V v :: env) e')
  | IR.Proj (i, n, e) ->
     F.GVar "project" $$ F.Lit (Int i) $$ F.Lit (Int n) $$ purify env e
  | IR.Dict l ->
     let rec enc = function
         [] -> F.GVar "Nil"
       | hd :: tl -> F.(GVar "::" $$ purify env hd $$ enc tl)
     in enc l

module U : sig
  type t
    = Var of int
    | Lam of t
    | App of t * t
    | Let of t * t
    | Lit of lit
    | Clo of int list * t
    | Dup of int list * t
    | GVar of string
    | Drop of int list * t
end = struct
  type t
    = Var of int
    | Lam of t
    | App of t * t
    | Let of t * t
    | Lit of lit
    | Clo of int list * t
    | Dup of int list * t
    | GVar of string
    | Drop of int list * t
end

let ($$) e e' = U.App (e, e')

let rec erase = function
    F.Var n -> U.Var n
  | F.TLam (_, e)
  | F.TApp (e, _) -> erase e
  | F.Lam (_, e) -> U.Lam (erase e)
  | F.App (e, e') -> erase e $$ erase e'
  | F.Lit l -> U.Lit l
  | F.Let (e, e') -> U.Let (erase e, erase e')
  | F.GVar v -> U.GVar v
