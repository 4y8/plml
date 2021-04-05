open Type
open Syntax

let _ =
  let e, t, _ = infer_expr {vctx = ["<", Forall ([0], ["Ord", TVar 0], TFun (TVar 0, TFun (TVar 0, TCon("Bool", []))))]; dctx = []}
                  (Lam ("x", Lam ("y", App (App (Var "<", Var "x"), Var "y"))))
  in
  let e, t = gen e t in
  print_endline (Core.show_coreIR e);
  print_endline (show_scheme t)
