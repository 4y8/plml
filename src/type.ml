open Syntax

let ntvar = ref 0
let newtvar () = let n = !ntvar in ntvar := n + 1; n

let rec wrap_app e = function
    [] -> e
  | hd :: tl -> wrap_app (Core.App (e, hd)) tl

let rec wrap_tapp e = function
    [] -> e
  | hd :: tl -> wrap_tapp (Core.TApp (e, hd)) tl

let rec app_subst_stype s =
  let aux = app_subst_stype s in
  function
    TFun (l, r) -> TFun (app_subst_stype s l, app_subst_stype s r)
  | TCon (v, l) -> TCon (v, List.map aux l)
  | TVar n -> match List.assoc_opt n s with
                None -> TVar n
              | Some t -> t

let rec infer_expr env = function
    Var _ as e ->
    let OverType (d, t), e, s = infer_over env e in
    t, wrap_app e, s
and infer_over env = function
    Var _ as e ->
    let Forall (v, c, t), e, _ = infer_poly env e in
    let s = List.map (fun t -> t, TVar (newtvar ())) v in
    let _, l = List.split s in
    OverType (c, app_subst_stype s t), wrap_tapp e l, []
and infer_poly env = function
    Var n ->
    List.nth env.ve n, Core.Var n, []
