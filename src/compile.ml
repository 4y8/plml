open Closure
open Common

let compile_fun = Printf.sprintf "Value
%s(Value arg, Env env)
{
	%s
	return %s;
}
"

type world = { glocode : string; maincode : string; nlam : int; ids : (string * string) list }

let new_world = {
    glocode = "";
    maincode = "";
    nlam = 0;
    ids = ["primeqint", "primeqint"; "primaddint", "primaddint";
           "primmulint", "primmulint"; "primdivint", "primdivint";
           "primsubint", "primsubint"]
  }

let (>>=) l r =
  fun w ->
  match l w with
    None -> None
  | Some (v, w) -> r v w

let (let*) = (>>=)

let (>>) l r =
  fun w ->
  match l w with
    None -> None
  | Some w -> r w

let return v =
  fun w -> Some (v, w)

let fail =
  fun _ -> None

let void =
  fun w -> Some (w)

let add_code v { glocode; maincode; nlam; ids } =
  Some { glocode = glocode ^ v; maincode; nlam; ids }

let add_maincode v { glocode; maincode; nlam; ids } =
  Some { glocode = glocode; maincode = maincode ^ v; nlam; ids }

let new_lam () { glocode; maincode; nlam; ids } =
  Some ("l" ^ string_of_int nlam, {glocode;  maincode; nlam = nlam + 1; ids})

let compile_id v w =
  let add_var v v' {glocode; maincode; nlam; ids} =
    Some {glocode; maincode; nlam; ids = (v, v') :: ids}
  in
  match List.assoc_opt v w.ids with
    Some v -> return v w
  | None ->
     let m =
       let* v' = new_lam () in add_var v v' >> return v'
     in m w

let compile_lit = function
    Syntax.Int n -> return $ Printf.sprintf "(mkint(%d))" n
  | Syntax.Bool true -> return "(mkint(1))"
  | Syntax.Bool false -> return "(mkint(0))"

let rec compile_list = function
    [] -> return []
  | hd :: tl ->
     let* v, _ = compile_expr hd in
     let* tl = compile_list tl in
     return (v :: tl)

and compile_expr = function
    Lit c ->
     let* c = compile_lit c in
     return (c, "")
  | Arg -> return ("arg", "")
  | GVar v -> let* v = compile_id v in return (v, "")
  | Dup (l, e) ->
     let* e, b = compile_expr e in
     let* l = compile_list l in
     let l = List.map (Printf.sprintf "dup(%s);") l in
     let dup = List.fold_left (^) "" l in
     return (e, dup ^ b)
  | Drop (l, e) ->
     let* e, b = compile_expr e in
     let* l = compile_list l in
     let l = List.map (Printf.sprintf "drop(%s);") l in
     let drop = List.fold_left (^) "" l in
     return (e, drop ^ b)
  | Env n -> return (Printf.sprintf "(env.p[%d])" n, "")
  | App (e, e') ->
     let* f, fp = compile_expr e in
     let* x, xp = compile_expr e' in
     return (Printf.sprintf "(call_closure(%s, %s))" f x, fp ^ xp)
  | Clo (l, e) ->
     let* v, body = compile_expr e in
     let* f = new_lam () in
     add_code (compile_fun f body v) >>
     let* preb, env =
       match l with
         [] -> return ("", "null_env")
       | l ->
          let env = f ^ "_env" in
          let preb =
            Printf.sprintf "Env %s = alloc_env(%d);\n"
              env (List.length l) in
          let rec get_env l acc n =
            match l with
              [] -> return acc
            | hd :: tl ->
               let* v, b = compile_expr hd in
               let addnv = Printf.sprintf "add_env(%s, %s, %d);\n" env v n in
               get_env tl (acc ^ b ^ addnv) (n + 1)
          in
          let* preb = get_env l preb 0 in
          return (preb, env)
     in
     return (Printf.sprintf "(mkclosure(%s, %s))" f env, preb)
  | _ -> return ("", "")

let compile_prog prog =
  let rec monadic_compile_prog = function
      [] -> void
    | (v, e) :: tl ->
       let* v = compile_id v in
       add_code (Printf.sprintf "Value %s;" v) >>
       let* e, b = compile_expr e in
       add_maincode (b ^ Printf.sprintf "%s = %s;" v e) >>
         monadic_compile_prog tl
  in
  match monadic_compile_prog prog new_world with
    None -> raise Lexer.Invalid_program
  | Some (w) ->
     let main = List.assoc "main" w.ids in
     "#include \"c/plml.h\"\n" ^ w.glocode ^
       Printf.sprintf "int main(){%s return (long)%s >> 1;}"
         w.maincode main
