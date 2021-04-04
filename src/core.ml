open Syntax

type coreIR
  = Var of string
  | Deb of int
  | Lam of string * stype * coreIR
  | App of coreIR * coreIR
  | TVar of int
  | TLam of int * coreIR
  | TApp of coreIR * stype
  | Proj of int * int * coreIR
  | Dict of coreIR list
  | DLam of stype * coreIR
  | DVar of stype
