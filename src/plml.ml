open Type
open Perceus
open Syntax

let bool = TCon ("Bool", [])
let int = TCon ("Int", [])

let (=>) l r = TFun (l, r)

let env0 =
  {vctx =
     ["<", Forall ([0], [TCon ("Ord", [TVar 0])], TVar 0 => (TVar 0 => bool));
      "succ", Forall ([], [], int => int);
      "primeqint", Forall ([], [], int => (int => bool))];
   dctx = [Forall ([], [], TCon ("Ord", [int])), "ordint"];
   cctx = []}

let test = "class Eq a where
	(==) : a -> a -> Bool
instance Eq Int where
	(==) = primeqint
id x =
	x"

let _ =
  let t = Lexer.lexer test in
  List.iter (fun x -> print_endline (Lexer.show x)) t;
  let p = Parser.parser t in
  List.iter (fun x -> print_endline (show_toplevel x)) p;
  let tpd = infer_prog env0 p in
  List.iter (fun (v, e) -> Printf.printf "%s: %s\n" v (Core.IR.show e)) tpd;
  let e, t, _ =
    infer_expr env0
      (Lam ("x", Var "x"))
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
  let c = Closure.closure_convert [] p in
  print_endline (Closure.show c);
  let c = Compile.compile_expr c Compile.new_world in
  match c with
    Some (c, w) ->
     let p =
       w.glocode ^
         Compile.compile_fun "main" (snd c) (fst c)
     in print_string p;
  | None -> raise Lexer.Invalid_program
