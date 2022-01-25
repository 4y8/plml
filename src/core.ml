open Syntax

module IR : sig
  type t
    = Var of string
    | Lam of string * stype * t
    | App of t * t
    | Let of string * t * t
    | Lit of lit
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
    | Lit of lit
    | TLam of int * t
    | TApp of t * stype
    | Proj of int * int * t
    | Dict of t list
    | DLam of stype * t
    | DVar of stype
  [@@deriving show]
end


module U : sig
  type t
    = Var of int
    | Lam of t
    | App of t * t
    | Lit of lit
    | Clo of int list * t
    | Dup of int list * t
    | GVar of string
    | Drop of int list * t
    | Dict of t list
    | Proj of int * t 
  [@@deriving show]
  val ($$) : t -> t -> t
end = struct
  type t
    = Var of int
    | Lam of t
    | App of t * t
    | Lit of lit
    | Clo of int list * t
    | Dup of int list * t
    | GVar of string
    | Drop of int list * t
    | Dict of t list
    | Proj of int * t 
  [@@deriving show]
  let ($$) e e' = App (e, e')
end

type vtype = V of string | D of stype

let ($$) e e' = U.App (e, e')

let rec erase env = function
    IR.Var v ->
    let i = Common.index (V v) env in
    if i = -1 then U.GVar v else U.Var i
  | IR.App (e, e') -> erase env e $$ erase env e'
  | IR.Lam (v, _, e) -> U.Lam (erase (V v :: env) e)
  | IR.TApp (e, _)
  | IR.TLam (_, e) -> erase env e
  | IR.DVar d -> U.Var (Common.index (D d) env)
  | IR.DLam (d, e) -> U.Lam (erase (D d :: env) e)
  | IR.Let (v, e, e') -> U.Lam (erase (V v :: env) e') $$ erase env e
  | IR.Proj (i, _, e) ->
     U.Proj (i, erase env e)
  | IR.Dict l ->
     U.Dict (List.map (erase env) l)
  | IR.Lit l -> U.Lit l

