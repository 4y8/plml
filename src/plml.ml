open Type
open Syntax

let bool = TCon ("Bool", [])
let int = TCon ("Int", [])

let (=>) l r = TFun (l, r)

let env0 = {
    vctx =
      ["<", Forall ([0], [TCon ("Ord", [TVar 0])], TVar 0 => (TVar 0 => bool));
       "primeqint", Forall ([], [], int => (int => bool));
       "primaddint", Forall ([], [], int => (int => int));
       "primmulint", Forall ([], [], int => (int => int));
       "primdivint", Forall ([], [], int => (int => int));
       "primsubint", Forall ([], [], int => (int => int))];
    dctx = [];
    cctx = []
  }

let test2 = "class Eq a where
	(==) : a -> a -> Bool
instance Eq Int where
	(==) = primeqint
class Num a where
	(+) : a -> a -> a
	(-) : a -> a -> a
	(*) : a -> a -> a
	(/) : a -> a -> a
instance Num Int where
	(+) = primaddint
	(-) = primsubint
	(*) = primmulint
	(/) = primdivint
main = \\x y -> x + y
"

let test = "id = \\x -> x
main = let x = \\x y -> x in id 6
"

let _ =
  let t = Lexer.lexer test in
  let p = Parser.parser t in
  let prog = infer_prog env0 p in
  let eprog = Common.smap (Core.erase []) prog in
  let lprog = Common.smap (Perceus.annlin [] []) eprog in
  let cprog = Common.smap (Closure.closure_convert []) lprog in
  print_string (Compile.compile_prog cprog)
