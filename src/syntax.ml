type stype
  = TVar of int
  | TFun of stype * stype
  | TCon of string * stype list

type context = (string * stype) list
type overtype = OverType of context * stype
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

type tcontext = {
    ve : (string * polytype) list;
    ie : (string * polytype) list;
    lie : (stype * stype) list;
  }
