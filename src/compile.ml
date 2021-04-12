open Closure
open Common

let compile_fun = Printf.sprintf "value
%s(Value arg, Env env)
{
	%s
	return %s;
}
"

type world = { glocode : string; nlam : int; ids : (string * string) list }

let new_world = {
    glocode = "";
    nlam = 0;
    ids = ["primeqint", "primeqint"; "primaddint", "primaddint";
           "primmultint", "primmultint"; "primdivint", "primdivint";
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

let add_code v { glocode; nlam; ids } =
  Some { glocode = glocode ^ v; nlam; ids }

let new_lam () { glocode; nlam; ids } =
  Some ("l" ^ string_of_int nlam, {glocode; nlam = nlam + 1; ids})

let compile_id v w =
  let add_var v v' {glocode; nlam; ids} =
    Some {glocode; nlam; ids = (v, v') :: ids}
  in
  match List.assoc_opt v w.ids with
    Some v -> return v w
  | None ->
     let m =
       let* v' = new_lam () in add_var v' v >> return v'
     in m w

let compile_lit = function
    Syntax.Int n -> return $ Printf.sprintf "(mkint(%d))" n
  | Syntax.Bool true -> return "(mkint(1))"
  | Syntax.Bool false -> return "(mkint(0))"

let rec compile_expr = function
    Lit c ->
     let* c = compile_lit c in
     return (c, "")
  | Arg -> return ("arg", "")
  | GVar v -> let* v = compile_id v in return (v, "")
  | Dup (_, e) -> compile_expr e (* TODO - manage drop and dup *)
  | Drop (_, e) -> compile_expr e
  | Env n -> return (Printf.sprintf "(env[%d])" n, "")
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
         [] -> return ("", "NULL")
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

let compile_prog prog =
  let rec monadic_compile_prog = function
      [] -> return ""
    | (v, e) :: tl ->
       let* e, b = compile_expr e in
       let* tl = monadic_compile_prog tl in
       let* v = compile_id v in
       return $ compile_fun v b e ^ tl
  in
  match monadic_compile_prog prog new_world with
    None -> raise Lexer.Invalid_program
  | Some (p, w) ->
     w.glocode ^ p
