type stype
  = TVar of int
  | TFun of stype * stype
  | TCon of stype list

type overtype = OverType of (string * stype) list * stype
type context = (string * int) list 
type polytype = Forall of int list * context * stype

type expr
  = Var of string
  | Lam of string * expr
  | App of expr * expr
  | Let of string * expr * expr

type classdecl = {
    ctx: context;
    name: string;
    members: (string * stype) list
  }

type instdecl = {
    ctx: context;
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
