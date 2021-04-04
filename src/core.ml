open Syntax

type coreIR
  = Var of string
  | Deb of int
  | Lam of string * stype * coreIR
  | App of coreIR * coreIR
  | TLam of int * coreIR
  | TApp of coreIR * stype
  | Proj of int * int * coreIR
  | Dict of coreIR list
  | DLam of (string * stype) * coreIR
  | DVar of string * stype
