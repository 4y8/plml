type stype
  = TVar of int
  | TFun of stype * stype
  | TCon of string * stype list
[@@deriving show]

type constr = (string * stype) list
[@@deriving show]
type scheme = Forall of int list * constr * stype
[@@deriving show]

type expr
  = Var of string
  | Lam of string * expr
  | App of expr * expr
  | Let of string * expr * expr

type classdecl = {
    ctx: constr;
    name: string;
    members: (string * stype) list
  }

type instdecl = {
    ctx: constr;
    name: string;
    ty: stype;
    decls: (string * expr) list
  }

type exprdecl = {
    name: string;
    expr: expr
  }

type toplevel
  = Expr of exprdecl
  | Class of classdecl
  | Instance of instdecl

type env = {
    vctx : (string * scheme) list;
    dctx : (scheme * string) list
  }
