type stype
  = TVar of int
  | TFun of stype * stype
  | TCon of string * stype list
[@@deriving show]

type constr = stype list
[@@deriving show]
type scheme = Forall of int list * constr * stype
[@@deriving show]

type lit = Int of int | Bool of bool
[@@deriving show]

type expr
  = Var of string
  | Lam of string * expr
  | App of expr * expr
  | Let of string * expr * expr
[@@deriving show]

type classdecl = {
    name: string;
    members: (string * stype) list
  }
[@@deriving show]

type instdecl = {
    ty: scheme;
    decls: (string * expr) list
  }
[@@deriving show]

type exprdecl = {
    name: string;
    expr: expr
  }
[@@deriving show]

type toplevel
  = Expr of exprdecl
  | Class of classdecl
  | Instance of instdecl
[@@deriving show]

type env = {
    vctx : (string * scheme) list;
    dctx : (scheme * string) list;
    cctx : classdecl list
  }
