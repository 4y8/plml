open Type
open Perceus
open Syntax

let bool = TCon ("Bool", [])
let int = TCon ("Int", [])

let (=>) l r = TFun (l, r)

let env0 =
  {vctx =
     ["<", Forall ([0], [TCon ("Ord", [TVar 0])], TVar 0 => (TVar 0 => bool));
      "succ", Forall ([], [], int => int)];
   dctx = [Forall ([], [], TCon ("Ord", [int])), "ordint"];
   cctx = []}

let _ =
  let e, t, _ =
    infer_expr env0
      (*
        (Lam ("x", Lam ("y", App (App (Var "<", Var "x"), Var "y"))))
       *)
      (Lam ("x", Let ("y", App (Var "succ", Var "x"),
                      App (App (Var "<", Var "x"), Var "y"))))
  in
  let e, t = gen e t in
  print_endline (Core.IR.show e);
  print_endline (show_scheme t);
  let p = Core.purify [] e in
  print_endline (Core.F.show p);
  let p = Core.erase p in
  print_endline (Core.U.show p);
  let p = annlin [] [] p in
  print_endline (Core.U.show p);
