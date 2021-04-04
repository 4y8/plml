open Syntax

let ntvar = ref 0
let newtvar () = let n = !ntvar in ntvar := n + 1; n

let rec wrap_lam e = function
    [] -> e
  | (v, t) :: tl -> wrap_lam (Core.Lam (v, t, e)) tl

let rec wrap_tlam e = function
    [] -> e
  | hd :: tl -> wrap_tlam (Core.TLam (hd, e)) tl

let rec wrap_dlam e = function
    [] -> e
  | hd :: tl -> wrap_dlam (Core.DLam (hd, e)) tl

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

let rec free_tvar_type bnd = function
    TVar v when List.mem v bnd -> []
  | TVar v -> [v]
  | TFun (l, r) -> free_tvar_type bnd l @ free_tvar_type bnd r
  | TCon (_, t) -> List.fold_left (@) [] (List.map (free_tvar_type bnd) t)

let free_tvar_expr =
  let rec aux bnd = function
      Core.TVar v when List.mem v bnd -> []
    | Core.TVar v -> [v]
    | Core.App (l, r) -> aux bnd l @ aux bnd r
    | Core.Lam (_, t, e) -> free_tvar_type bnd t @ aux bnd e
    | Core.TLam (v, e) -> aux (v :: bnd) e
    | Core.DLam (t, e)
    | Core.TApp (e, t) -> aux bnd e @ free_tvar_type bnd t
    | Core.Var _
    | Core.Deb _ -> []
    | Core.Proj (_, _, e) -> aux bnd e
    | Core.Dict l -> List.fold_left (@) [] (List.map (aux bnd) l)
    | Core.DVar t -> free_tvar_type bnd t
  in aux []

let free_dvar =
  let rec aux bnd = function
      Core.DVar t when List.mem t bnd -> []
    | Core.DVar t -> [t]
    | Core.DLam (t, e) -> aux (t :: bnd) e
    | Core.TVar _
    | Core.Deb _
    | Core.Var _ -> []
    | Core.Proj (_, _, e)
    | Core.TLam (_, e)
    | Core.TApp (e, _)
    | Core.Lam (_, _, e) -> aux bnd e
    | Core.App (l, r) -> aux bnd l @ aux bnd r
    | Core.Dict l -> List.fold_left (@) [] (List.map (aux bnd) l)
  in aux []

let inst e t =
  let fvt = free_tvar_expr e in
  let fvd = free_dvar e in
  

let rec infer_expr env = function
    Var v as e ->
     match List.assoc_opt v env.lve with
       None ->
       let OverType (d, t), e, s = infer_over env e in
       t, wrap_app e, s
     | Some t -> t, Core.Var v, []
and infer_over env = function
    Var _ as e ->
    let Forall (v, c, t), e, _ = infer_poly env e in
    let s = List.map (fun t -> t, TVar (newtvar ())) v in
    let _, l = List.split s in
    OverType (c, app_subst_stype s t), wrap_tapp e l, []
and infer_poly env = function
    Var v ->
    List.assoc v env.ve, Core.Var v, []
