open Syntax
open Core
open Common

exception Occurs_check
exception Unify_error
exception Missing_dictionnary
exception Invalid_class_declaration

let ntvar = ref 0
let newtvar () = let n = !ntvar in ntvar := n + 1; n

let dictvar = ref 0
let newdictvar () = let n = !ntvar in ntvar := n + 1; "dict" ^ (string_of_int n)

let gettyname = function
    TCon (v, _) -> v
  | _ -> failwith "The program did not know it was impossible so it did it"

let wrap_lam e l =
  let rec aux e = function
    [] -> e
  | (v, t) :: tl -> aux (IR.Lam (v, t, e)) tl
  in aux e (List.rev l)

let wrap_tlam e l =
  let rec aux e = function
    [] -> e
  | hd :: tl -> aux (IR.TLam (hd, e)) tl
  in aux e (List.rev l)

let wrap_dlam e l =
  let rec aux e = function
    [] -> e
  | hd :: tl -> aux (IR.DLam (hd, e)) tl
  in aux e (List.rev l)

let rec wrap_app e = function
    [] -> e
  | hd :: tl -> wrap_app (IR.App (e, hd)) tl

let rec wrap_tapp e = function
    [] -> e
  | hd :: tl -> wrap_tapp (IR.TApp (e, hd)) tl

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

let app_subst_env s {vctx; dctx; cctx} =
  {vctx = app_subst_ctx s vctx; dctx; cctx}

let app_subst_constr s =
  List.map (app_subst_stype s)

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
      IR.Let (_, e, e')
    | IR.App (e, e') -> aux bnd e @: aux bnd e'
    | IR.Lam (_, t, e) -> ftv_type bnd t @: aux bnd e
    | IR.TLam (v, e) -> aux (v :: bnd) e
    | IR.DLam (t, e)
    | IR.TApp (e, t) -> aux bnd e @: ftv_type bnd t
    | IR.Lit _
    | IR.Var _ -> []
    | IR.Proj (_, _, e) -> aux bnd e
    | IR.Dict l -> List.fold_left (@:) [] (List.map (aux bnd) l)
    | IR.DVar t -> ftv_type bnd t
  in aux []

let fdv =
  let rec aux bnd = function
      IR.DVar t when List.mem t bnd -> []
    | IR.DVar t -> [t]
    | IR.DLam (t, e) -> aux (t :: bnd) e
    | IR.Lit _
    | IR.Var _ -> []
    | IR.Proj (_, _, e)
    | IR.TLam (_, e)
    | IR.TApp (e, _)
    | IR.Lam (_, _, e) -> aux bnd e
    | IR.Let (_, e, e')
    | IR.App (e, e') -> aux bnd e @ aux bnd e'
    | IR.Dict l -> List.fold_left (@:) [] (List.map (aux bnd) l)
  in aux []

let bind v t =
  if List.mem v (ftv_type [] t) then raise Occurs_check
  else [v, t]

let rec unify t t' =
  match t, t' with
    TCon (v, l), TCon (v', l') when v = v' ->
     List.fold_left (@@) [] (List.map2 unify l l')
  | TFun (l, r), TFun (l', r') ->
     let s = unify l l' in
     let s' = unify (app_subst_stype s r) (app_subst_stype s r') in
     s @@ s'
  | t, TVar v | TVar v, t -> bind v t
  | _ -> raise Unify_error

let rec unify_left t t' =
  match t, t' with
    TCon (v, l), TCon (v', l') when v = v' ->
     List.fold_left (@@) [] (List.map2 unify_left l l')
  | TFun (l, r), TFun (l', r') ->
     let s = unify_left l l' in
     let s' = unify_left (app_subst_stype s r) r' in
     s @@ s'
  | TVar v, t -> bind v t
  | _ -> raise Unify_error

let rec dict env = function
    (TCon (k, [t])) ->
     let rec fd_dict = function
         [] -> IR.DVar (TCon (k, [t]))
       | (Forall (_, _, TCon (k', _)), _) :: tl when k <> k' ->
          fd_dict tl
       | (Forall (b, c, TCon (_, [t'])), v) :: tl ->
          begin
           try
             let s = unify_left t' t in
             let t' = app_subst_stype s t' in
             let c' = app_subst_constr s  c in
             let b' = List.filter
                        (fun x -> List.assoc_opt x s = None) b in
             let e, _ = inst env v (Forall (b', c', t')) in
             e
           with _ -> fd_dict tl
          end
       | _ -> raise Not_found
     in
     fd_dict env.dctx
  | _ -> raise Not_found (* Can't happen *)


and inst env v (Forall (b, c, t)) =
  let l = List.map (fun _ -> newtvar ()) b in
  let ta = (List.map (fun v -> TVar v) l) in
  let s = List.combine b ta in
  let t = app_subst_stype s t in
  let c = app_subst_constr s c in
  let d = List.map (fun t -> dict env t) c in
  wrap_app (wrap_tapp (IR.Var v) ta) d, t

let rec app_subst_expr s env e =
  let aux = app_subst_expr s env in
  match e with
    IR.Var v -> IR.Var v
  | IR.Lit l -> IR.Lit l
  | IR.App (e, e') -> IR.App (aux e, aux e')
  | IR.Lam (v, t, e) -> IR.Lam (v, app_subst_stype s t, aux e)
  | IR.Let (v, e, e') -> IR.Let (v, aux e, aux e')
  | IR.TLam (n, e) ->
     IR.TLam (n, app_subst_expr (remove_assoc_all n s) env e)
  | IR.TApp (e, t) -> IR.TApp (aux e, app_subst_stype s t)
  | IR.DVar t ->
     let t = app_subst_stype s t in
     dict env t
  | IR.Dict l -> IR.Dict (List.map aux l)
  | IR.DLam (t, e) ->
     IR.DLam (app_subst_stype s t, aux e)
  | IR.Proj (i, n, e) -> IR.Proj (i, n, aux e)

let gen e t =
  let ft = ftv_expr e in
  let fd = fdv e in
  wrap_tlam (wrap_dlam e fd) ft, Forall (ft, fd, t)

let add_var {vctx; dctx; cctx} v t =
  {vctx = (v, t) :: vctx; dctx; cctx}

let add_dict {vctx; dctx; cctx} v t =
  {vctx; dctx = (t, v) :: dctx; cctx}

let add_class {vctx; dctx; cctx} c =
  {vctx; dctx; cctx = c :: cctx}

let rec infer_expr env = function
    Var v ->
     let e, t = inst env v (List.assoc v env.vctx) in
     e, t, []
  | App (e, e') ->
     let e, t, s = infer_expr env e in
     let e', t', s' = infer_expr (app_subst_env s env) e' in
     let tv = TVar (newtvar ()) in
     let s = (unify (app_subst_stype s t) (TFun (t', tv))) @@ s' @@ s in
     IR.App (app_subst_expr s env e, app_subst_expr s env e'),
     app_subst_stype s tv, s
  | Lam (v, e) ->
     let tv = TVar (newtvar ()) in
     let e, t', s = infer_expr (add_var env v (Forall ([], [], tv))) e in
     let t = app_subst_stype s tv in
     IR.Lam (v, t, e), TFun (t, t'), s
  | Let (v, e, e') ->
     let e, t, s = infer_expr env e in
     let e, t = gen e t in
     let e', t', s' = infer_expr (add_var (app_subst_env s env) v t) e' in
     IR.Let (v, e, e'), t', s' @@ s
  | Lit l ->
     match l with
       Bool _ -> IR.Lit l, TCon ("bool", []), []
     | Int _ -> IR.Lit l, TCon ("int", []), []

let infer_class (c : classdecl) =
  let kt = TCon (c.name, [TVar 0]) in
  let n = List.length c.members in
  let infer_decl i (v, t) =
    v,
    Forall ([0], [kt], t),
    IR.(TLam (0, Lam ("dvar", kt, Proj (i, n, Var "dvar"))))
  in
  List.mapi infer_decl c.members

let infer_instance env (i : instdecl) =
  let v = newdictvar () in
  let rec fdclass v = function
      [] -> raise Not_found
    | {name = v'; members = _} as d :: _ when v = v' ->
       d
    | _ :: tl -> fdclass v tl
  in
  let Forall (_, _, t) = i.ty in
  let d = fdclass (gettyname t) env.cctx in
  let rec map2class i = function
      [] when i = [] -> []
    | [] -> raise Invalid_class_declaration
    | (v, t) :: tl ->
       let decl = List.assoc v i in
       let e, t', _ = infer_expr env decl in
       let s = unify t t' in
       let e = app_subst_expr s env e in
       let i = List.remove_assoc v i in
       e :: map2class i tl
  in
  let dct = map2class i.decls d.members in
  v, i.ty, IR.Dict dct

let infer_decl env (d : exprdecl) =
  let e, t, _ = infer_expr env d.expr in
  let e, t = gen e t in
  d.name, t, e

let rec infer_prog env =
  function
    [] -> []
  | Expr e :: tl ->
     let v, t, e = infer_decl env e in
     (v, e) :: infer_prog (add_var env v t) tl
  | Class c :: tl ->
     let l = infer_class c in
     let rec getenv env expr = function
         [] -> List.rev expr, env
       | (v, t, e) :: tl ->
          getenv (add_var env v t) ((v, e) :: expr) tl
     in
     let l, env = getenv env [] l in
     l @ infer_prog (add_class env c) tl
  | Instance i :: tl ->
     let v, t, d = infer_instance env i in
     (v, d) :: infer_prog (add_dict env v t) tl
