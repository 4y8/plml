open Type
open Syntax

let _ =
  let e, t, _ = infer_expr {vctx = []} (Lam ("x", Var "x")) in
  let e, t = gen e t in
  print_endline (Core.show_coreIR e);
  print_endline (show_scheme t)
