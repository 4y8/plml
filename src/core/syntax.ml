type typeF
  = TVar of int
  | Forall of typeF
  | TFun of typeF * typeF

type coreF
  = Var of int
  | Lam of typeF * coreF
  | App of coreF * coreF
  | TLam of coreF
  | TApp of coreF * typeF

type valueF
  = VLam of typeF * coreF
  | VTLam of coreF
