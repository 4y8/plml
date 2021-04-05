open Syntax
open Common

exception Occurs_check
exception Unify_error
exception Missing_dictionnary

let ntvar = ref 0
let newtvar () = let n = !ntvar in ntvar := n + 1; n

let wrap_lam e l =
  let rec aux e = function
    [] -> e
  | (v, t) :: tl -> aux (Core.Lam (v, t, e)) tl
  in aux e (List.rev l)

let wrap_tlam e l =
  let rec aux e = function
    [] -> e
  | hd :: tl -> aux (Core.TLam (hd, e)) tl
  in aux e (List.rev l)

let wrap_dlam e l =
  let rec aux e = function
    [] -> e
  | hd :: tl -> aux (Core.DLam (hd, e)) tl
  in aux e (List.rev l)

let rec wrap_app e = function
    [] -> e
  | hd :: tl -> wrap_app (Core.App (e, hd)) tl

let rec wrap_tapp e = function
    [] -> e
  | hd :: tl -> wrap_tapp (Core.TApp (e, hd)) tl

let rec app_subst_stype s =
  function
    TFun (l, r) -> TFun (app_subst_stype s l, app_subst_stype s r)
  | TCon (v, l) -> TCon (v, List.map (app_subst_stype s) l)
  | TVar n -> match List.assoc_opt n s with
                None -> TVar n
              | Some t -> t

let app_subst_scheme s (Forall (b, c, t)) =
  let s = List.filter (fun (f, _) -> not (List.mem f b)) s in
  Forall (b, c, app_subst_stype s t)

let app_subst_ctx =
  let rec aux acc s = function
      [] -> List.rev acc
    | (v, t) :: tl -> aux ((v, app_subst_scheme s t) :: acc) s tl
  in aux []

let app_subst_env s {vctx; dctx} =
  {vctx = app_subst_ctx s vctx; dctx}

let rec app_subst_constr s = function
    [] -> []
  | (v, t) :: tl -> (v, app_subst_stype s t) :: app_subst_constr s tl

let rec app_subst_expr s =
  function
    Core.Var v -> Core.Var v
  | Core.Deb n -> Core.Deb n
  | Core.App (e, e') -> Core.App (app_subst_expr s e, app_subst_expr s e')
  | Core.Lam (v, t, e) -> Core.Lam (v, app_subst_stype s t, app_subst_expr s e)
  | Core.Let (v, e, e') -> Core.Let (v, app_subst_expr s e, app_subst_expr s e')
  | Core.TLam (n, e) ->
     Core.TLam (n, app_subst_expr (remove_assoc_all n s) e)
  | Core.TApp (e, t) -> Core.TApp (app_subst_expr s e, app_subst_stype s t)
  | Core.DVar (v, t) -> Core.DVar (v, app_subst_stype s t)
  | Core.Dict l -> Core.Dict (List.map (app_subst_expr s) l)
  | Core.DLam ((v, t), e) ->
     Core.DLam ((v, app_subst_stype s t), app_subst_expr s e)
  | Core.Proj (i, n, e) -> Core.Proj (i, n, app_subst_expr s e)

let (@@) s s' =
  let v, t = List.split s' in
  let t = List.map (app_subst_stype s) t in
  (List.combine v t) @ s

let rec ftv_type bnd = function
    TVar v when List.mem v bnd -> []
  | TVar v -> [v]
  | TFun (l, r) -> ftv_type bnd l @: ftv_type bnd r
  | TCon (_, t) -> List.fold_left (@:) [] (List.map (ftv_type bnd) t)

let ftv_expr =
  let rec aux bnd = function
      Core.Let (_, e, e')
    | Core.App (e, e') -> aux bnd e @: aux bnd e'
    | Core.Lam (_, t, e) -> ftv_type bnd t @: aux bnd e
    | Core.TLam (v, e) -> aux (v :: bnd) e
    | Core.DLam ((_, t), e)
    | Core.TApp (e, t) -> aux bnd e @: ftv_type bnd t
    | Core.Var _
    | Core.Deb _ -> []
    | Core.Proj (_, _, e) -> aux bnd e
    | Core.Dict l -> List.fold_left (@:) [] (List.map (aux bnd) l)
    | Core.DVar (_, t) -> ftv_type bnd t
  in aux []

let fdv =
  let rec aux bnd = function
      Core.DVar (s, t) when List.mem (s, t) bnd -> []
    | Core.DVar (s, t) -> [s, t]
    | Core.DLam (t, e) -> aux (t :: bnd) e
    | Core.Deb _
    | Core.Var _ -> []
    | Core.Proj (_, _, e)
    | Core.TLam (_, e)
    | Core.TApp (e, _)
    | Core.Lam (_, _, e) -> aux bnd e
    | Core.Let (_, e, e')
    | Core.App (e, e') -> aux bnd e @ aux bnd e'
    | Core.Dict l -> List.fold_left (@:) [] (List.map (aux bnd) l)
  in aux []

let rec unify t t' =
  let bind v t =
    if List.mem v (ftv_type [] t) then raise Occurs_check
    else [v, t]
  in
  match t, t' with
    TCon (v, l), TCon (v', l') when v = v' ->
     List.fold_left (@@) [] (List.map2 unify l l')
  | TFun (l, r), TFun (l', r') ->
     let s = unify l l' in
     let s' = unify (app_subst_stype s r) (app_subst_stype s r') in
     s @@ s'
  | t, TVar v | TVar v, t -> bind v t
  | _ -> raise Unify_error

let rec dict env k = function
    (TVar _) as t -> Core.DVar (k, t)
  | t ->
     let rec fd_dict = function
         [] -> raise Missing_dictionnary
       | (_, k', _) :: tl when k <> k' ->
          fd_dict tl
       | ((Forall (_, _, t')) as s, _, v) :: tl ->
          try
            let _ = unify t t' in
            s, v
          with _ -> fd_dict tl
     in
     let t, v = fd_dict env.dctx in
     let e, _ = inst env v t in
     e

and inst env v (Forall (b, c, t)) =
  let l = List.map (fun _ -> newtvar ()) b in
  let ta = (List.map (fun v -> TVar v) l) in
  let s = List.combine b ta in
  let t = app_subst_stype s t in
  let c = app_subst_constr s c in
  let d = List.map (fun (f, s) -> dict env f s) c in
  wrap_app (wrap_tapp (Core.Var v) ta) d, t

let gen e t =
  let ft = ftv_expr e in
  let fd = fdv e in
  wrap_tlam (wrap_dlam e fd) ft, Forall (ft, fd, t)

let add_var {vctx; dctx} v t =
  {vctx = (v, t) :: vctx; dctx}

let rec infer_expr env = function
    Var v ->
     let e, t = inst env v (List.assoc v env.vctx) in
     e, t, []
  | App (e, e') ->
     let e, t, s = infer_expr env e in
     let e', t', s' = infer_expr (app_subst_env s env) e' in
     let tv = TVar (newtvar ()) in
     let s = (unify (app_subst_stype s t) (TFun (t', tv))) @@ s' @@ s in
     Core.App (e, e'), app_subst_stype s tv, s
  | Lam (v, e) ->
     let tv = TVar (newtvar ()) in
     let e, t', s = infer_expr (add_var env v (Forall ([], [], tv))) e in
     let t = app_subst_stype s tv in
     Core.Lam (v, t, e), TFun (t, t'), s
  | Let (v, e, e') ->
     let e, t, s = infer_expr env e in
     let e, t = gen e t in
     let e', t', s' = infer_expr (add_var (app_subst_env s env) v t) e' in
     Core.Let (v, e, e'), t', s' @@ s
