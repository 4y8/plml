type typeF
  = TVar of int
  | Forall of int list * typeF
  | TFun of typeF * typeF

type coreF
  = Var of int
  | Lam of typeF * coreF
  | App of coreF * coreF
  | TLam of coreF
  | TApp of coreF * typeF
  | Proj of int * int * coreF
  | Dict of coreF list
