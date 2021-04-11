open Type
open Syntax

let bool = TCon ("Bool", [])
let int = TCon ("Int", [])

let (=>) l r = TFun (l, r)

let env0 =
  {vctx =
     ["<", Forall ([0], [TCon ("Ord", [TVar 0])], TVar 0 => (TVar 0 => bool));
      "succ", Forall ([], [], int => int);
      "primeqint", Forall ([], [], int => (int => bool));
      "primaddint", Forall ([], [], int => (int => int));
      "primmultint", Forall ([], [], int => (int => int));
      "primdivint", Forall ([], [], int => (int => int));
      "primsubint", Forall ([], [], int => (int => int))];
   dctx = [];
   cctx = []}

let test = "class Eq a where
	(==) : a -> a -> Bool
instance Eq Int where
	(==) = primeqint
id x =
	x
class Num a where
	(+) : a -> a -> a
	(-) : a -> a -> a
	(*) : a -> a -> a
	(/) : a -> a -> a
instance Num Int where
	(+) = primaddint
	(-) = primsubint
	(*) = primmultint
	(/) = primdivint
"


let _ =
  let t = Lexer.lexer test in
  List.iter (fun x -> print_endline (Lexer.show x)) t;
  let p = Parser.parser t in
  List.iter (fun x -> print_endline (show_toplevel x)) p;
  let prog = infer_prog env0 p in
  List.iter (fun (v, e) -> Printf.printf "%s: %s\n" v (Core.IR.show e)) prog;
  let pprog = Common.smap (Core.purify []) prog in
  let eprog = Common.smap (Core.erase) pprog in
  let lprog = Common.smap (Perceus.annlin [] []) eprog in
  let cprog = Common.smap (Closure.closure_convert []) lprog in
  List.iter (fun (v, e) -> Printf.printf "%s: %s\n" v (Closure.show e)) cprog;
  print_string (Compile.compile_prog cprog)
