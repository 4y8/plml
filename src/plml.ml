open Type
open Syntax

let bool = TCon ("Bool", [])
let int = TCon ("Int", [])

let (=>) l r = TFun (l, r)

let env0 =
  {vctx =
     ["<", Forall ([0], ["Ord", TVar 0], TVar 0 => (TVar 0 => bool));
      "succ", Forall ([], [], int => int)];
   dctx = [Forall ([], [], TCon ("Ord", [int])), "ordint"]}

let _ =
  let e, t, _ =
    infer_expr env0
      (*
        (Lam ("x", Lam ("y", App (App (Var "<", Var "x"), Var "y"))))
       *)
      (Lam ("x", Let ("y", App (Var "succ", Var "x"), App (App (Var "<", Var "x"), Var "y"))))
  in
  let e, t = gen e t in
  print_endline (Core.show_coreIR e);
  print_endline (show_scheme t)
