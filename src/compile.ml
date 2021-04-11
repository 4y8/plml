open Closure
open Common

let compile_fun = Printf.sprintf "value
%s(Value arg, Env env)
{
	%s
	return %s;
}
"

type world = { glocode : string; nlam : int }

let new_world = { glocode = ""; nlam = 0 }

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

let add_code v { glocode; nlam } =
  Some { glocode = glocode ^ v; nlam }

let new_lam () { glocode; nlam } =
  Some ("lam" ^ string_of_int nlam, {glocode; nlam = nlam + 1})

let compile_lit = function
    Syntax.Int n -> return $ Printf.sprintf "(mkint(%d))" n
  | Syntax.Bool true -> return "(mkint(1))"
  | Syntax.Bool false -> return "(mkint(0))"

let rec compile_expr = function
    Lit c ->
     let* c = compile_lit c in
     return (c, "")
  | Arg -> return ("arg", "")
  | GVar v -> return (Printf.sprintf "%s" v, "")
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
       return $ compile_fun v b e ^ tl
  in
  match monadic_compile_prog prog new_world with
    None -> raise Lexer.Invalid_program
  | Some (p, w) ->
     w.glocode ^ p
